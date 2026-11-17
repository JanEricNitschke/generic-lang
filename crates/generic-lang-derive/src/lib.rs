//! Derive macros used by `generic-lang-lib`.

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields, parse_macro_input};

/// Derive location-insensitive equality for AST nodes.
///
/// Fields marked with `#[loc_less(ignore)]` are skipped. All other fields
/// must implement `crate::compiler::ast_eq::LocationLessEq`.
#[proc_macro_derive(LocationLessEq, attributes(loc_less))]
pub fn derive_location_less_eq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match try_derive(input) {
        Ok(tokens) => tokens,
        Err(error) => error.to_compile_error().into(),
    }
}

#[allow(clippy::too_many_lines)]
fn try_derive(input: DeriveInput) -> syn::Result<TokenStream> {
    let name = input.ident;

    let body = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => {
                let ignored = fields
                    .named
                    .iter()
                    .map(is_ignored)
                    .collect::<syn::Result<Vec<_>>>()?;
                let comparisons = fields
                    .named
                    .iter()
                    .zip(ignored)
                    .filter(|(_, ignored)| !ignored)
                    .map(|(field, _)| {
                        let name = field.ident.as_ref().expect("named field");
                        quote! { self.#name.loc_eq(&other.#name) }
                    });
                quote! { true #( && #comparisons)* }
            }
            Fields::Unnamed(fields) => {
                let comparisons = fields
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(index, field)| Ok((index, field, is_ignored(field)?)))
                    .collect::<syn::Result<Vec<_>>>()?
                    .into_iter()
                    .filter(|(_, _, ignored)| !ignored)
                    .map(|(index, _, _)| {
                        let index = syn::Index::from(index);
                        quote! { self.#index.loc_eq(&other.#index) }
                    });
                quote! { true #( && #comparisons)* }
            }
            Fields::Unit => quote! { true },
        },
        Data::Enum(data) => {
            let arms = data
                .variants
                .iter()
                .map(|variant| {
                    let variant_name = &variant.ident;
                    Ok(match &variant.fields {
                        Fields::Unit => quote! {
                            (Self::#variant_name, Self::#variant_name) => true
                        },
                        Fields::Named(fields) => {
                            let pairs = fields
                                .named
                                .iter()
                                .map(|field| Ok((field, is_ignored(field)?)))
                                .collect::<syn::Result<Vec<_>>>()?
                                .into_iter()
                                .filter(|(_, ignored)| !ignored)
                                .map(|(field, _)| {
                                    let name = field.ident.as_ref().expect("named field");
                                    let a = format_ident!("a_{name}");
                                    let b = format_ident!("b_{name}");
                                    (name, a, b)
                                })
                                .collect::<Vec<_>>();
                            let a_patterns = pairs.iter().map(|(name, a, _)| quote! { #name: #a });
                            let b_patterns = pairs.iter().map(|(name, _, b)| quote! { #name: #b });
                            let comparisons =
                                pairs.iter().map(|(_, a, b)| quote! { #a.loc_eq(#b) });
                            quote! {
                                (
                                    Self::#variant_name { #(#a_patterns,)* .. },
                                    Self::#variant_name { #(#b_patterns,)* .. },
                                ) => true #( && #comparisons)*
                            }
                        }
                        Fields::Unnamed(fields) => {
                            let entries = fields
                                .unnamed
                                .iter()
                                .enumerate()
                                .map(|(index, field)| Ok((index, field, is_ignored(field)?)))
                                .collect::<syn::Result<Vec<_>>>()?;
                            let a_patterns = entries.iter().map(|(index, _, ignored)| {
                                if *ignored {
                                    quote! { _ }
                                } else {
                                    let a = format_ident!("a{index}");
                                    quote! { #a }
                                }
                            });
                            let b_patterns = entries.iter().map(|(index, _, ignored)| {
                                if *ignored {
                                    quote! { _ }
                                } else {
                                    let b = format_ident!("b{index}");
                                    quote! { #b }
                                }
                            });
                            let comparisons = entries
                                .iter()
                                .filter(|(_, _, ignored)| !ignored)
                                .map(|(index, _, _)| {
                                    let a = format_ident!("a{index}");
                                    let b = format_ident!("b{index}");
                                    quote! { #a.loc_eq(#b) }
                                });
                            quote! {
                                (
                                    Self::#variant_name(#(#a_patterns,)*),
                                    Self::#variant_name(#(#b_patterns,)*),
                                ) => true #( && #comparisons)*
                            }
                        }
                    })
                })
                .collect::<syn::Result<Vec<_>>>()?;
            quote! {
                match (self, other) {
                    #(#arms,)*
                    _ => false,
                }
            }
        }
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                name,
                "unions cannot derive LocationLessEq",
            ));
        }
    };

    Ok(quote! {
        impl crate::compiler::ast_eq::LocationLessEq for #name {
            fn loc_eq(&self, other: &Self) -> bool {
                #body
            }
        }
    }
    .into())
}

fn is_ignored(field: &syn::Field) -> syn::Result<bool> {
    let mut ignored = false;
    for attr in &field.attrs {
        if !attr.path().is_ident("loc_less") {
            continue;
        }

        let mut saw_ignore = false;
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("ignore") {
                saw_ignore = true;
                ignored = true;
                Ok(())
            } else {
                Err(meta.error("expected `ignore`"))
            }
        })?;
        if !saw_ignore {
            return Err(syn::Error::new_spanned(
                attr,
                "expected `#[loc_less(ignore)]`",
            ));
        }
    }
    Ok(ignored)
}

/// Derive conversion of an internal AST node for the `ast` stdlib module.
///
/// - every struct becomes a class with all of its fields;
/// - every unit or struct-like enum variant becomes a class;
/// - a newtype variant is transparent unless its payload is a `Vec`, in which
///   case it becomes a class with one field;
/// - `variant_prefix` / `variant_suffix` adjust enum variant class names;
/// - `field = "..."` names the field of a `Vec` payload;
/// - `name = "..."` renames an exported field.
///
/// `semicolon_location` fields are compiler-only anchors and are not exported.
///
/// # Generated code
///
/// A struct:
///
/// ```ignore
/// #[derive(ToAst)]
/// struct Param {
///     name: String,
///     location: Location,
/// }
///
/// // impl ToAst for Param {
/// //     fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
/// //         converter.class("Param", vec![
/// //             ("name", ToAst::to_ast(&self.name, converter)),
/// //             ("location", ToAst::to_ast(&self.location, converter)),
/// //         ])
/// //     }
/// // }
/// ```
///
/// A unit variant and a struct-like variant:
///
/// ```ignore
/// #[derive(ToAst)]
/// enum StmtKind {
///     Break { label: Option<String> },
///     Return { value: Option<Box<Expr>> },
/// }
///
/// // Self::Break { label } => converter.class("Break", vec![
/// //     ("label", ToAst::to_ast(label, converter)),
/// // ]),
/// // Self::Return { value } => converter.class("Return", vec![
/// //     ("value", ToAst::to_ast(value, converter)),
/// // ]),
/// ```
///
/// A transparent newtype variant:
///
/// ```ignore
/// #[derive(ToAst)]
/// enum StmtKind {
///     Class(Box<ClassDecl>),
/// }
///
/// // Self::Class(value) => ToAst::to_ast(value, converter)
/// ```
///
/// A `Vec` newtype variant:
///
/// ```ignore
/// #[derive(ToAst)]
/// #[ast(variant_prefix = "Lambda")]
/// enum LambdaBody {
///     #[ast(field = "statements")]
///     Block(Vec<Stmt>),
/// }
///
/// // Self::Block(value) => converter.class("LambdaBlock", vec![
/// //     ("statements", ToAst::to_ast(value, converter)),
/// // ])
/// ```
///
/// # Reverse conversion
///
/// The same derive generates `FromAst`, which rebuilds the internal node from
/// an exported instance. Class names and field names come from the same
/// attributes as the forward direction.
///
/// A struct:
///
/// ```text
/// // impl FromAst for Param {
/// //     fn from_ast(importer, value) -> Result<Self, ImportError> {
/// //         importer.enter()?;
/// //         let result = (|| {
/// //             let value = importer.required(value, "Param")?;
/// //             let instance = importer.instance(value, "Param")?;
/// //             Ok(Self {
/// //                 name: <String as FromAst>::from_ast(
/// //                     importer,
/// //                     importer.field(instance, "name"),
/// //                 )?,
/// //                 location: <Location as FromAst>::from_ast(
/// //                     importer,
/// //                     importer.field(instance, "location"),
/// //                 )?,
/// //             })
/// //         })();
/// //         importer.leave();
/// //         result
/// //     }
/// // }
/// ```
///
/// A unit variant, a struct-like variant, and a `Vec` newtype variant:
///
/// ```text
/// // match importer.class_name(value) {
/// //     Some(class) => match class.as_str() {
/// //         "Break" => return Ok(Self::Break { label: ... }),
/// //         "Return" => {
/// //             let instance = importer.instance(value, "Return")?;
/// //             return Ok(Self::Return { value: ... });
/// //         }
/// //         "LambdaBlock" => {
/// //             let instance = importer.instance(value, "LambdaBlock")?;
/// //             return Ok(Self::Block(
/// //                 <Vec<Stmt> as FromAst>::from_ast(
/// //                     importer,
/// //                     importer.field(instance, "statements"),
/// //                 )?,
/// //             ));
/// //         }
/// //         _ => {}
/// //     },
/// // }
/// ```
///
/// A transparent newtype variant has no class of its own. Its payload is
/// exported directly, so the reverse arm matches the payload's class:
///
/// ```text
/// // "ClassDecl" => return Ok(Self::Class(
/// //     <Box<ClassDecl> as FromAst>::from_ast(importer, Some(value))?,
/// // )),
/// ```
///
/// Transparent payloads that export as raw values have no class to match:
///
/// ```text
/// // if let Value::String(_) = value {
/// //     return Ok(Self::Text(<String as FromAst>::from_ast(
/// //         importer,
/// //         Some(value),
/// //     )?));
/// // }
/// //
/// // // `ExprKind::Literal`: raw nil/bool/number/string/tuple fallback.
/// // return Ok(Self::Literal(<Literal as FromAst>::from_ast(
/// //     importer,
/// //     Some(value),
/// // )?));
/// ```
#[proc_macro_derive(ToAst, attributes(ast))]
pub fn derive_to_ast(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match try_derive_to_ast(input) {
        Ok(tokens) => tokens,
        Err(error) => error.to_compile_error().into(),
    }
}

/// Enum-level naming policy applied to every variant class.
#[derive(Default)]
struct EnumAttrs {
    prefix: Option<String>,
    suffix: Option<String>,
}

/// Attributes on a single enum variant.
#[derive(Default)]
struct VariantAttrs {
    /// Exported field name for a `Vec` payload.
    field: Option<String>,
}

/// Attributes on a struct or variant field.
#[derive(Default)]
struct FieldAttrs {
    /// Export this field under a different name.
    name: Option<String>,
}

/// Parse `#[ast(variant_prefix = "...", variant_suffix = "...")]`.
fn parse_enum_attrs(attrs: &[syn::Attribute]) -> syn::Result<EnumAttrs> {
    let mut parsed = EnumAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("ast") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("variant_prefix") {
                parsed.prefix = Some(meta.value()?.parse::<syn::LitStr>()?.value());
                Ok(())
            } else if meta.path.is_ident("variant_suffix") {
                parsed.suffix = Some(meta.value()?.parse::<syn::LitStr>()?.value());
                Ok(())
            } else {
                Err(meta.error("expected `variant_prefix` or `variant_suffix`"))
            }
        })?;
    }
    Ok(parsed)
}

/// Parse `#[ast(field = "...")]` on a variant.
fn parse_variant_attrs(attrs: &[syn::Attribute]) -> syn::Result<VariantAttrs> {
    let mut parsed = VariantAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("ast") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("field") {
                parsed.field = Some(meta.value()?.parse::<syn::LitStr>()?.value());
                Ok(())
            } else {
                Err(meta.error("expected `field`"))
            }
        })?;
    }
    Ok(parsed)
}

/// Parse `#[ast(name = "...")]` on a field.
fn parse_field_attrs(attrs: &[syn::Attribute]) -> syn::Result<FieldAttrs> {
    let mut parsed = FieldAttrs::default();
    for attr in attrs {
        if !attr.path().is_ident("ast") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("name") {
                parsed.name = Some(meta.value()?.parse::<syn::LitStr>()?.value());
                Ok(())
            } else {
                Err(meta.error("expected `name`"))
            }
        })?;
    }
    Ok(parsed)
}

/// `semicolon_location` is a compiler-only anchor, not an AST field.
fn is_compiler_only_field(field: &syn::Field) -> bool {
    field
        .ident
        .as_ref()
        .is_some_and(|ident| ident == "semicolon_location")
}

/// Whether a payload is a `Vec`, optionally behind a `Box`.
fn is_vec_type(ty: &syn::Type) -> bool {
    let syn::Type::Path(path) = ty else {
        return false;
    };
    let Some(segment) = path.path.segments.last() else {
        return false;
    };
    if segment.ident == "Vec" {
        return true;
    }
    if segment.ident != "Box" {
        return false;
    }
    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return false;
    };
    let Some(syn::GenericArgument::Type(inner)) = args.args.first() else {
        return false;
    };
    is_vec_type(inner)
}

/// Apply the enum-level prefix/suffix to a variant class name.
fn class_name(attrs: &EnumAttrs, variant: &syn::Ident) -> syn::LitStr {
    let name = format!(
        "{}{}{}",
        attrs.prefix.as_deref().unwrap_or_default(),
        variant,
        attrs.suffix.as_deref().unwrap_or_default(),
    );
    syn::LitStr::new(&name, variant.span())
}

/// Dispatch to the struct or enum implementation.
fn try_derive_to_ast(input: DeriveInput) -> syn::Result<TokenStream> {
    let name = input.ident;
    match input.data {
        Data::Struct(data) => derive_struct(&name, &data.fields),
        Data::Enum(data) => derive_enum(&name, &input.attrs, &data.variants),
        Data::Union(_) => Err(syn::Error::new_spanned(name, "unions cannot derive ToAst")),
    }
}

/// Generate `ToAst` and the field helper for a struct.
fn derive_struct(name: &syn::Ident, fields: &Fields) -> syn::Result<TokenStream> {
    let class = syn::LitStr::new(&name.to_string(), name.span());
    let Fields::Named(fields) = fields else {
        return Err(syn::Error::new_spanned(
            fields,
            "ToAst structs must have named fields",
        ));
    };

    let mut entries = Vec::new();
    let mut from_fields = Vec::new();
    for field in &fields.named {
        let attrs = parse_field_attrs(&field.attrs)?;
        let field_name = field.ident.as_ref().expect("named field");
        if is_compiler_only_field(field) {
            from_fields.push(quote! { #field_name: Default::default() });
            continue;
        }
        let default_name = field_name.to_string();
        let exported_name = attrs.name.as_deref().unwrap_or(&default_name);
        let exported_name = syn::LitStr::new(exported_name, field.span());
        entries.push(quote! {
            (#exported_name, ToAst::to_ast(&self.#field_name, converter))
        });
        let ty = &field.ty;
        from_fields.push(quote! {
            #field_name: <#ty as crate::ast_import::FromAst>::from_ast(
                importer,
                importer.field(instance, #exported_name),
            )?
        });
    }

    Ok(quote! {
        impl #name {
            pub(crate) fn ast_fields(
                &self,
                converter: &mut Converter<'_>,
            ) -> Vec<(&'static str, Value)> {
                vec![#(#entries),*]
            }
        }

        impl ToAst for #name {
            fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
                let fields = self.ast_fields(converter);
                converter.class(#class, fields)
            }
        }

        impl crate::ast_import::FromAst for #name {
            fn from_ast(
                importer: &mut crate::ast_import::Importer<'_>,
                value: Option<Value>,
            ) -> Result<Self, crate::ast_import::ImportError> {
                importer.enter()?;
                let result = (|| -> Result<Self, crate::ast_import::ImportError> {
                    let value = importer.required(value, #class)?;
                    let instance = importer.instance(value, #class)?;
                    Ok(Self { #(#from_fields),* })
                })();
                importer.leave();
                result
            }
        }

        inventory::submit! { crate::ast_export::AstClass(&[#class]) }
    }
    .into())
}

/// Strip one `Box<T>` layer, if present.
fn unbox_type(ty: &syn::Type) -> &syn::Type {
    let syn::Type::Path(path) = ty else {
        return ty;
    };
    let Some(segment) = path.path.segments.last() else {
        return ty;
    };
    if segment.ident != "Box" {
        return ty;
    }
    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return ty;
    };
    let Some(syn::GenericArgument::Type(inner)) = args.args.first() else {
        return ty;
    };
    inner
}

/// The last identifier of a path type.
fn type_ident(ty: &syn::Type) -> Option<&syn::Ident> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    path.path.segments.last().map(|segment| &segment.ident)
}

/// The class name a transparent newtype variant is exported as.
///
/// Transparent variants have no class of their own: `StmtKind::Class(
/// Box<ClassDecl>)` is exported as a `ClassDecl` instance,
/// `ExprKind::Tstring(Box<Tstring>)` as a `Tstring`, and so on. Reverse
/// dispatch therefore unboxes `Box<T>` and uses `T`'s class name as the match
/// key.
///
/// `String` and `Literal` payloads are different: they are exported as raw
/// values, not class instances, so they have no class name here. They are
/// handled by the raw-string and literal-fallback paths instead.
fn transparent_payload_class_name(ty: &syn::Type) -> Option<String> {
    let ident = type_ident(unbox_type(ty))?;
    if ident == "String" || ident == "Literal" {
        return None;
    }
    Some(ident.to_string())
}

/// Generate one `FromAst` field initializer.
fn from_ast_field(field: &syn::Field, exported_name: &syn::LitStr) -> proc_macro2::TokenStream {
    let field_name = field.ident.as_ref().expect("named field");
    if is_compiler_only_field(field) {
        return quote! { #field_name: Default::default() };
    }
    let ty = &field.ty;
    quote! {
        #field_name: <#ty as crate::ast_import::FromAst>::from_ast(
            importer,
            importer.field(instance, #exported_name),
        )?
    }
}

/// Generate `ToAst`, `FromAst`, and the class registration for an enum.
#[allow(clippy::too_many_lines)]
fn derive_enum(
    name: &syn::Ident,
    attrs: &[syn::Attribute],
    variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> syn::Result<TokenStream> {
    let enum_attrs = parse_enum_attrs(attrs)?;
    let mut classes = Vec::new();
    let mut arms = Vec::new();
    // Reverse arms are grouped by how the exported value identifies the
    // variant:
    //
    // - class_arms match `importer.class_name(value)`:
    //     "Break", "Return", "LambdaBlock", and transparent payload classes
    //     such as "ClassDecl" or "Tstring".
    // - raw_string_arms match a raw `Value::String`:
    //     currently only `FstringPart::Text`.
    // - fallback_arms are unconditional and catch all remaining raw values:
    //     currently only `ExprKind::Literal` (nil, bool, number, string, tuple).
    //
    // Class arms run first so class instances are never mistaken for raw
    // values. At most one fallback arm is allowed.
    let mut class_arms = Vec::new();
    let mut raw_string_arms = Vec::new();
    let mut fallback_arms = Vec::new();

    for variant in variants {
        let variant_name = &variant.ident;
        let variant_attrs = parse_variant_attrs(&variant.attrs)?;
        let class = class_name(&enum_attrs, variant_name);

        match &variant.fields {
            Fields::Unit => {
                classes.push(class.clone());
                arms.push(quote! {
                    Self::#variant_name => converter.class(#class, vec![])
                });
                class_arms.push(quote! {
                    #class => return Ok(Self::#variant_name)
                });
            }
            Fields::Named(fields) => {
                classes.push(class.clone());
                let mut bindings = Vec::new();
                let mut entries = Vec::new();
                let mut from_fields = Vec::new();
                for field in &fields.named {
                    let field_attrs = parse_field_attrs(&field.attrs)?;
                    let field_name = field.ident.as_ref().expect("named field");
                    if is_compiler_only_field(field) {
                        from_fields.push(quote! { #field_name: Default::default() });
                        continue;
                    }
                    let binding = quote::format_ident!("field_{field_name}");
                    bindings.push(quote! { #field_name: #binding });
                    let default_name = field_name.to_string();
                    let exported_name = field_attrs.name.as_deref().unwrap_or(&default_name);
                    let exported_name = syn::LitStr::new(exported_name, field.span());
                    entries.push(quote! {
                        (#exported_name, ToAst::to_ast(#binding, converter))
                    });
                    from_fields.push(from_ast_field(field, &exported_name));
                }
                arms.push(quote! {
                    Self::#variant_name { #(#bindings),*, .. } => {
                        let fields = vec![#(#entries),*];
                        converter.class(#class, fields)
                    }
                });
                class_arms.push(quote! {
                    #class => {
                        let instance = importer.instance(value, #class)?;
                        return Ok(Self::#variant_name { #(#from_fields),* });
                    }
                });
            }
            Fields::Unnamed(fields) => {
                if fields.unnamed.len() != 1 {
                    return Err(syn::Error::new_spanned(
                        variant,
                        "newtype variants must have exactly one field",
                    ));
                }
                let field = &fields.unnamed[0];
                if is_vec_type(&field.ty) {
                    classes.push(class.clone());
                    let field_name = variant_attrs.field.as_deref().unwrap_or("value");
                    let field_name = syn::LitStr::new(field_name, variant.span());
                    arms.push(quote! {
                        Self::#variant_name(value) => {
                            let value = ToAst::to_ast(value, converter);
                            converter.class(#class, vec![(#field_name, value)])
                        }
                    });
                    let ty = &field.ty;
                    class_arms.push(quote! {
                        #class => {
                            let instance = importer.instance(value, #class)?;
                            let value = <#ty as crate::ast_import::FromAst>::from_ast(
                                importer,
                                importer.field(instance, #field_name),
                            )?;
                            return Ok(Self::#variant_name(value));
                        }
                    });
                } else {
                    arms.push(quote! {
                        Self::#variant_name(value) => ToAst::to_ast(value, converter)
                    });
                    let ty = &field.ty;
                    let payload = unbox_type(ty);
                    // A transparent newtype variant has no exported class of
                    // its own; its payload is exported directly. The reverse
                    // arm therefore depends on the payload shape:
                    //
                    // - String payload: exported as a raw string
                    //   (`FstringPart::Text`), so match `Value::String`.
                    // - Literal payload: exported as a raw literal
                    //   (`ExprKind::Literal`), so use the unconditional
                    //   fallback after all class arms.
                    // - Any other payload: exported as its payload class
                    //   (`StmtKind::Class` -> "ClassDecl",
                    //   `ExprKind::Tstring` -> "Tstring"), so match that class.
                    if type_ident(payload).is_some_and(|ident| ident == "String") {
                        raw_string_arms.push(quote! {
                            if let Value::String(_) = value {
                                return Ok(Self::#variant_name(
                                    <#ty as crate::ast_import::FromAst>::from_ast(
                                        importer,
                                        Some(value),
                                    )?,
                                ));
                            }
                        });
                    } else if type_ident(payload).is_some_and(|ident| ident == "Literal") {
                        fallback_arms.push(quote! {
                            return Ok(Self::#variant_name(
                                <#ty as crate::ast_import::FromAst>::from_ast(
                                    importer,
                                    Some(value),
                                )?,
                            ));
                        });
                    } else if let Some(payload_class) = transparent_payload_class_name(ty) {
                        let payload_class = syn::LitStr::new(&payload_class, field.span());
                        class_arms.push(quote! {
                            #payload_class => return Ok(Self::#variant_name(
                                <#ty as crate::ast_import::FromAst>::from_ast(
                                    importer,
                                    Some(value),
                                )?,
                            ))
                        });
                    }
                }
            }
        }
    }

    if fallback_arms.len() > 1 {
        return Err(syn::Error::new_spanned(
            name,
            "at most one transparent fallback variant is supported",
        ));
    }

    let registration = if classes.is_empty() {
        quote! {}
    } else {
        quote! {
            inventory::submit! { crate::ast_export::AstClass(&[#(#classes),*]) }
        }
    };

    let enum_name = syn::LitStr::new(&name.to_string(), name.span());
    let from_fallback = if fallback_arms.is_empty() {
        quote! { Err(importer.unexpected(#enum_name, value)) }
    } else {
        quote! { #(#fallback_arms)* }
    };

    Ok(quote! {
        impl ToAst for #name {
            fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
                match self {
                    #(#arms),*
                }
            }
        }

        impl crate::ast_import::FromAst for #name {
            fn from_ast(
                importer: &mut crate::ast_import::Importer<'_>,
                value: Option<Value>,
            ) -> Result<Self, crate::ast_import::ImportError> {
                importer.enter()?;
                let result = (|| -> Result<Self, crate::ast_import::ImportError> {
                    let value = importer.required(value, #enum_name)?;
                    if let Some(class) = importer.class_name(value) {
                        match class.as_str() {
                            #(#class_arms),*,
                            _ => {}
                        }
                    }
                    #(#raw_string_arms)*
                    #from_fallback
                })();
                importer.leave();
                result
            }
        }

        #registration
    }
    .into())
}

#[cfg(test)]
mod tests {
    use super::is_ignored;

    #[test]
    fn recognizes_the_ignore_attribute() {
        let field: syn::Field = syn::parse_quote!(#[loc_less(ignore)] value: u8);
        assert!(is_ignored(&field).expect("valid attribute"));
    }

    #[test]
    fn rejects_unknown_loc_less_arguments() {
        let field: syn::Field = syn::parse_quote!(#[loc_less(foo)] value: u8);
        assert_eq!(
            is_ignored(&field).unwrap_err().to_string(),
            "expected `ignore`"
        );
    }

    #[test]
    fn rejects_empty_loc_less_attributes() {
        let field: syn::Field = syn::parse_quote!(#[loc_less] value: u8);
        assert_eq!(
            is_ignored(&field).unwrap_err().to_string(),
            "expected attribute arguments in parentheses: #[loc_less(...)]"
        );
    }
}
