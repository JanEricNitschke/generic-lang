//! Derive macros used by `generic-lang-lib`.

use proc_macro::TokenStream;
use quote::{format_ident, quote};
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
