use crate::derive::*;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct OwnedGenerator;

impl DeriveGenerator for OwnedGenerator {
    fn complete(input: &syn::DeriveInput, tokens: TokenStream) -> TokenStream {
        let input_ident = &input.ident;
        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        quote! {
            impl #impl_generics crate::ast::owned::Owned for #input_ident #ty_generics #where_clause {
                type Owned = #input_ident<'static>;

                fn owned(&self) -> Self::Owned {
                    #tokens
                }
            }
        }
    }
}

impl StructGenerator for OwnedGenerator {
    fn generate(input: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let fields = strukt
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        quote! {
            #input {
                #(
                    #fields: self.#fields.owned(),
                )*
            }
        }
    }
}

impl MatchEnumGenerator for OwnedGenerator {
    fn case_named(
        input: &syn::Ident,
        variant: &syn::Ident,
        fields: &syn::FieldsNamed,
    ) -> TokenStream {
        let fields: Vec<_> = fields
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect();

        quote! {
            #input::#variant {
                #(#fields,)*
            } => {
                #input::#variant {
                    #(
                        #fields: #fields.owned(),
                    )*
                }
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

        quote! {
            #input::#variant(
                #(#fields,)*
            ) => {
                #input::#variant(
                    #(
                        #fields.owned(),
                    )*
                )
            }
        }
    }

    fn case_unit(input: &syn::Ident, variant: &syn::Ident) -> TokenStream {
        quote! {
            #input::#variant => #input::#variant
        }
    }
}
