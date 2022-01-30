use crate::derive::*;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashMap;

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

#[derive(Debug, PartialEq)]
enum VisitHint {
    Contains(String),
    Skip,
    SkipVisitSelf,
    VisitAs(String),
}

impl Hint for VisitHint {
    fn key_value(key: String, value: String) -> Option<Self> {
        if key == "visit_as" {
            Some(VisitHint::VisitAs(value))
        } else if key == "contains" {
            Some(VisitHint::Contains(value))
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

impl VisitGenerator {
    fn visit_fields(data_fields: &syn::Fields, prefix: TokenStream) -> TokenStream {
        let mut fields = Vec::new();
        let mut contains = HashMap::new();

        for field in data_fields
            .iter()
            .filter(|field| search_hint("visit", &field.attrs) != Some(VisitHint::Skip))
        {
            let ident = field.ident.as_ref().unwrap();
            let token_stream = quote! { #prefix#ident };

            if let Some(VisitHint::Contains(contains_node)) = search_hint("visit", &field.attrs) {
                contains.insert(contains_node, ident);
            } else if let Some(contains_me) = contains.remove(&ident.to_string()) {
                fields.push(quote! {
                    #prefix#contains_me.tokens.0
                });

                fields.push(token_stream);

                fields.push(quote! {
                    #prefix#contains_me.tokens.1
                });
            } else {
                fields.push(token_stream);
            }
        }

        assert!(
            contains.is_empty(),
            "#[visit(contains = \"...\")] used in wrong order: {:?}",
            contains
        );

        quote! {
            #(visit!(#fields, visitor);)*
        }
    }
}

impl DeriveGenerator for VisitGenerator {
    fn complete(input: &syn::DeriveInput, tokens: TokenStream) -> TokenStream {
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
                        visit_self!(#visit_as);
                    },
                    quote! {
                        visit_self!(#visit_as_end);
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
                        visit_self!(#ssself);
                    },
                    quote! {
                        visit_self!(#ssself_end);
                    },
                )
            }
        };

        quote! {
            #[allow(unused_macros)]
            impl #impl_generics crate::visitors::Visit for #input_ident #ty_generics #where_clause {
                fn visit<V: crate::visitors::Visitor>(&self, visitor: &mut V) {
                    macro_rules! visit {
                        ($visit_what: expr, $visitor: expr) => {
                            $visit_what.visit($visitor);
                        }
                    }

                    macro_rules! visit_self {
                        ($name: ident) => {
                            visitor.$name(self);
                        }
                    }

                    macro_rules! set_self {
                        ($value: expr) => {
                            $value;
                        }
                    }

                    macro_rules! if_visit {
                        ({ $($used: expr)* } else { $($unused: expr)* }) => {
                            {
                                $($used;)*
                            }
                        }
                    }

                    #visit_self
                    #tokens
                    #visit_self_end
                }
            }

            #[allow(unused_macros)]
            impl #impl_generics crate::visitors::VisitMut for #input_ident #ty_generics #where_clause {
                fn visit_mut<V: crate::visitors::VisitorMut>(mut self, visitor: &mut V) -> Self {
                    macro_rules! visit {
                        ($visit_what: expr, $visitor: expr) => {
                            $visit_what = $visit_what.visit_mut($visitor);
                        }
                    }

                    macro_rules! visit_self {
                        ($name: ident) => {
                            self = visitor.$name(self);
                        }
                    }

                    macro_rules! set_self {
                        ($value: expr) => {
                            self = $value;
                        }
                    }

                    macro_rules! if_visit {
                        ({ $($unused: expr)* } else { $($used: expr)* }) => {
                            $($used)*
                        }
                    }

                    #visit_self
                    #tokens
                    #visit_self_end
                    self
                }
            }
        }
    }
}

impl StructGenerator for VisitGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        Self::visit_fields(&strukt.fields, quote! {self.})
    }
}

impl MatchEnumGenerator for VisitGenerator {
    fn complete(input: TokenStream) -> TokenStream {
        quote! {
            set_self!(#input);
        }
    }

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
                if_visit!({
                    #(
                        #fields.visit(visitor)
                    )*
                } else {
                    #input::#variant {
                        #(
                            #fields: #fields.visit_mut(visitor),
                        )*
                    }
                })
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
                if_visit!({
                    #(
                        #fields.visit(visitor)
                    )*
                } else {
                    #input::#variant(
                        #(
                            #fields.visit_mut(visitor),
                        )*
                    )
                })
            }
        }
    }
}
