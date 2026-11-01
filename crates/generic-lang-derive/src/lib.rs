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
        Data::Struct(data) => derive_struct(name, &data.fields),
        Data::Enum(data) => derive_enum(name, &input.attrs, &data.variants),
        Data::Union(_) => Err(syn::Error::new_spanned(name, "unions cannot derive ToAst")),
    }
}

/// Generate `ToAst` and the field helper for a struct.
fn derive_struct(name: syn::Ident, fields: &Fields) -> syn::Result<TokenStream> {
    let class = syn::LitStr::new(&name.to_string(), name.span());
    let Fields::Named(fields) = fields else {
        return Err(syn::Error::new_spanned(
            fields,
            "ToAst structs must have named fields",
        ));
    };

    let mut entries = Vec::new();
    for field in &fields.named {
        let attrs = parse_field_attrs(&field.attrs)?;
        let field_name = field.ident.as_ref().expect("named field");
        if is_compiler_only_field(field) {
            continue;
        }
        let default_name = field_name.to_string();
        let exported_name = attrs.name.as_deref().unwrap_or(&default_name);
        let exported_name = syn::LitStr::new(exported_name, field.span());
        entries.push(quote! {
            (#exported_name, ToAst::to_ast(&self.#field_name, converter))
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

        inventory::submit! { crate::ast_export::AstClass(&[#class]) }
    }
    .into())
}

/// Generate `ToAst` and the class registration for an enum.
fn derive_enum(
    name: syn::Ident,
    attrs: &[syn::Attribute],
    variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
) -> syn::Result<TokenStream> {
    let enum_attrs = parse_enum_attrs(attrs)?;
    let mut classes = Vec::new();
    let mut arms = Vec::new();

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
            }
            Fields::Named(fields) => {
                classes.push(class.clone());
                let mut bindings = Vec::new();
                let mut entries = Vec::new();
                for field in &fields.named {
                    let field_attrs = parse_field_attrs(&field.attrs)?;
                    if is_compiler_only_field(field) {
                        continue;
                    }
                    let field_name = field.ident.as_ref().expect("named field");
                    let binding = quote::format_ident!("field_{field_name}");
                    bindings.push(quote! { #field_name: #binding });
                    let default_name = field_name.to_string();
                    let exported_name = field_attrs.name.as_deref().unwrap_or(&default_name);
                    let exported_name = syn::LitStr::new(exported_name, field.span());
                    entries.push(quote! {
                        (#exported_name, ToAst::to_ast(#binding, converter))
                    });
                }
                arms.push(quote! {
                    Self::#variant_name { #(#bindings),*, .. } => {
                        let fields = vec![#(#entries),*];
                        converter.class(#class, fields)
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
                } else {
                    arms.push(quote! {
                        Self::#variant_name(value) => ToAst::to_ast(value, converter)
                    });
                }
            }
        }
    }

    let registration = if classes.is_empty() {
        quote! {}
    } else {
        quote! {
            inventory::submit! { crate::ast_export::AstClass(&[#(#classes),*]) }
        }
    };

    Ok(quote! {
        impl ToAst for #name {
            fn to_ast(&self, converter: &mut Converter<'_>) -> Value {
                match self {
                    #(#arms),*
                }
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
