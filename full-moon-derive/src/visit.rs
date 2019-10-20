use crate::derive::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

// Not 100% accurate, but it is to full-moon's codebase
fn snake_case(pascal_case: &str) -> String {
    let mut chars = pascal_case.chars();
    let mut result = chars.next().unwrap().to_ascii_lowercase().to_string();

    for character in chars {
        if character.is_ascii_uppercase() {
            result.push('_');
            result.push(character.to_ascii_lowercase());
        } else {
            result.push(character);
        }
    }

    result
}

#[derive(PartialEq)]
enum VisitHint {
    Skip,
	SkipVisitSelf,
    VisitAs(String),
}

impl Hint for VisitHint {
	fn key_value(key: String, value: String) -> Option<Self> {
		if key == "visit_as" {
			Some(VisitHint::VisitAs(value))
		} else {
			None
		}
	}

	fn unit(name: String) -> Option<Self> {
		match name.as_str() {
			"skip" => Some(VisitHint::Skip),
			"skip_visit_self" => Some(VisitHint::SkipVisitSelf),
			_ => None,
		}
	}
}

pub struct VisitGenerator;

impl DeriveGenerator for VisitGenerator {
    fn complete(input: &syn::DeriveInput, tokens: TokenStream) -> TokenStream {
        let lifetime = input
            .generics
            .lifetimes()
            .next()
            .expect("must derive something with a lifetime");

        let input_ident = &input.ident;
        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        let (visit_self, visit_self_end) = match search_hint("visit", &input.attrs) {
            Some(VisitHint::SkipVisitSelf) => (quote! {}, quote! {}),
            Some(VisitHint::VisitAs(visit_as)) => {
                let visit_as_end =
                    syn::Ident::new(&format!("visit_{}_end", visit_as), input_ident.span());
                let visit_as = syn::Ident::new(&format!("visit_{}", visit_as), input_ident.span());

                (
                    quote! {
                        visitor.#visit_as(self);
                    },
                    quote! {
                        visitor.#visit_as_end(self);
                    },
                )
            }
            _ => {
                // name of self in snake_case
                let ssself = syn::Ident::new(
                    &format!("visit_{}", snake_case(&input_ident.to_string())),
                    input_ident.span(),
                );

                let ssself_end = syn::Ident::new(
                    &format!("visit_{}_end", snake_case(&input_ident.to_string())),
                    input_ident.span(),
                );

                (
                    quote! {
                        visitor.#ssself(self);
                    },
                    quote! {
                        visitor.#ssself_end(self);
                    },
                )
            }
        };

        quote! {
            impl #impl_generics crate::visitors::Visit<#lifetime> for #input_ident #ty_generics #where_clause {
                fn visit<V: crate::visitors::Visitor<#lifetime>>(&self, visitor: &mut V) {
                    macro_rules! visit {
                        ($visit_what: expr, $visitor: expr) => {
                            $visit_what.visit($visitor);
                        }
                    }

                    #visit_self
                    #tokens
                    #visit_self_end
                }
            }

            impl #impl_generics crate::visitors::VisitMut<#lifetime> for #input_ident #ty_generics #where_clause {
                fn visit_mut<V: crate::visitors::VisitorMut<#lifetime>>(&mut self, visitor: &mut V) {
                    macro_rules! visit {
                        ($visit_what: expr, $visitor: expr) => {
                            $visit_what.visit_mut($visitor);
                        }
                    }

                    #visit_self
                    #tokens
                    #visit_self_end
                }
            }
        }
    }
}

impl StructGenerator for VisitGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let fields = strukt
            .fields
            .iter()
            .filter(|field| search_hint("visit", &field.attrs) != Some(VisitHint::Skip))
			.map(|field| field.ident.as_ref());

        quote! {
            #(visit!(self.#fields, visitor);)*
        }
    }
}

impl MatchEnumGenerator for VisitGenerator {
    fn case_named(
        input: &syn::Ident,
        variant: &syn::Ident,
        named: &syn::FieldsNamed,
    ) -> TokenStream {
        let fields: Vec<_> = named
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect();

        quote! {
            #input::#variant {
                #(#fields,)*
            } => {
                #(
                    visit!(#fields, visitor);
                )*
            }
        }
    }

    fn case_unnamed(
        input: &syn::Ident,
        variant: &syn::Ident,
        fields: &syn::FieldsUnnamed,
    ) -> TokenStream {
        let fields: Vec<_> = fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(index, _)| format_ident!("__self_{}", index))
            .collect();
        let fields = &fields;

        quote! {
            #input::#variant(
                #(#fields,)*
            ) => {
                #(
                    visit!(#fields, visitor);
                )*
            }
        }
    }
}
