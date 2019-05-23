extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Visit, attributes(visit))]
pub fn derive_visit(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let input_ident = &input.ident;

    let expanded = match &input.data {
        syn::Data::Enum(enumm) => {
            let mut cases = Vec::new();

            for variant in &enumm.variants {
                let variant_ident = &variant.ident;
                match &variant.fields {
                    syn::Fields::Named(named) => {
                        let mut fields = Vec::new();
                        let mut fields_to_visit = Vec::new();

                        for field in &named.named {
                            let mut skip = false;

                            for attr in &field.attrs {
                                if let Ok(syn::Meta::List(list)) = attr.parse_meta() {
                                    if list.ident == "visit" {
                                        if let syn::NestedMeta::Meta(syn::Meta::Word(word)) =
                                            list.nested
                                                .first()
                                                .expect("visit attribute cannot be empty")
                                                .value()
                                        {
                                            if word == "skip" {
                                                skip = true;
                                                break;
                                            }
                                        }
                                    }
                                }
                            }

                            let ident = field.ident.as_ref().unwrap();

                            if skip {
                                fields.push(quote! { #ident: _ });
                            } else {
                                fields.push(quote! { #ident });
                                fields_to_visit.push(ident);
                            }
                        }

                        cases.push(quote! {
                            #input_ident::#variant_ident {
                                #(#fields,)*
                            } => {
                                #(
                                    #fields_to_visit.visit(visitor);
                                )*
                            }
                        });
                    }

                    syn::Fields::Unnamed(fields) => {
                        let fields: Vec<_> = fields
                            .unnamed
                            .iter()
                            .enumerate()
                            .map(|(index, _)| {
                                syn::Ident::new(&format!("__self_{}", index), variant_ident.span())
                            })
                            .collect();
                        let fields = &fields;

                        cases.push(quote! {
                            #input_ident::#variant_ident(
                                #(#fields,)*
                            ) => {
                                #(
                                    #fields.visit(visitor);
                                )*
                            }
                        });
                    }

                    syn::Fields::Unit => {}
                }
            }

            quote! {
                match self {
                    #(#cases)*
                    _ => {}
                }
            }
        }

        syn::Data::Struct(strukt) => {
            quote! {}
        }

        other => panic!("don't know how to derive Visit from {:?}", other),
    };

    let lifetime = input
        .generics
        .params
        .first()
        .and_then(|param| match param.value() {
            syn::GenericParam::Lifetime(lifetime) => Some(lifetime),
            _ => None,
        })
        .expect("must derive something with a lifetime");

    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let source = quote! {
        impl #impl_generics crate::visitors::Visit<#lifetime> for #input_ident #ty_generics #where_clause {
            fn visit<V: crate::visitors::Visitor<#lifetime>>(&self, visitor: &mut V) {
                #expanded
            }
        }

        impl #impl_generics crate::visitors::VisitMut<#lifetime> for #input_ident #ty_generics #where_clause {
            fn visit<V: crate::visitors::VisitorMut<#lifetime>>(&mut self, visitor: &mut V) {
                #expanded
            }
        }
    };

    TokenStream::from(source)
}
