use crate::derive::*;

use proc_macro2::TokenStream;
use quote::quote;

#[derive(PartialEq)]
enum NodeHint {
    FullRange,
}

impl Hint for NodeHint {
	fn unit(name: String) -> Option<Self> {
		if name == "full_range" {
			Some(NodeHint::FullRange)
		} else {
			None
		}
	}
}

pub struct NodeGenerator;

impl DeriveGenerator for NodeGenerator {
    fn complete(input: &syn::DeriveInput, tokens: TokenStream) -> TokenStream {
        let input_ident = &input.ident;
        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        quote! {
            impl #impl_generics crate::node::Node for #input_ident #ty_generics #where_clause {
                fn start_position(&self) -> Option<crate::tokenizer::Position> {
                    Some(#tokens?.0)
                }

                fn end_position(&self) -> Option<crate::tokenizer::Position> {
                    Some(#tokens?.1)
                }
            }

            impl #impl_generics crate::private::Sealed for #input_ident #ty_generics #where_clause {}
        }
    }
}

impl StructGenerator for NodeGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let fields = strukt
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        let full_range = strukt
            .fields
            .iter()
            .find(|field| search_hint("node", &field.attrs) == Some(NodeHint::FullRange));

        if let Some(full_range) = full_range {
            let ident = full_range.ident.as_ref().unwrap();
            quote! {
                self.#ident.range()
            }
        } else {
            let (mut start_position, mut end_position) = (
                Vec::with_capacity(fields.len()),
                Vec::with_capacity(fields.len()),
            );

            for field in &fields {
                start_position.push(quote! {
                    .or_else(|| {
                        self.#field.start_position()
                    })
                });
            }

            for field in fields.iter().rev() {
                end_position.push(quote! {
                    .or_else(|| {
                        self.#field.end_position()
                    })
                });
            }

            quote! {
                Some((None#(#start_position)*?, None#(#end_position)*?))
            }
        }
    }
}

impl MatchEnumGenerator for NodeGenerator {
    fn case_named(
        input: &syn::Ident,
        variant: &syn::Ident,
        named: &syn::FieldsNamed,
    ) -> TokenStream {
        let fields = named
            .named
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        let full_range = named
            .named
            .iter()
            .find(|field| search_hint("node", &field.attrs) == Some(NodeHint::FullRange));

        let body = if let Some(full_range) = full_range {
            let ident = full_range.ident.as_ref().unwrap();
            quote! {
                #ident.range()
            }
        } else {
            let (mut start_position, mut end_position) = (
                Vec::with_capacity(fields.len()),
                Vec::with_capacity(fields.len()),
            );

            for field in &fields {
                start_position.push(quote! {
                    .or_else(|| {
                        #field.start_position()
                    })
                });
            }

            for field in fields.iter().rev() {
                end_position.push(quote! {
                    .or_else(|| {
                        #field.end_position()
                    })
                });
            }

            quote! {
                Some((None#(#start_position)*?, None#(#end_position)*?))
            }
        };

        quote! {
            #input::#variant {
                #(#fields,)*
            } => {
                #body
            }
        }
    }

    fn case_unnamed(
        input: &syn::Ident,
        variant: &syn::Ident,
        fields: &syn::FieldsUnnamed,
    ) -> TokenStream {
        let fields = &fields.unnamed;

        if fields.len() == 1 {
            quote! {
                #input::#variant(inner) => {
                    Some((inner.start_position()?, inner.end_position()?))
                }
            }
        } else {
            let fields_count = fields.len() - 1;
            let field_match: Vec<_> = fields
                .iter()
                .enumerate()
                .map(|(index, _)| {
                    syn::Ident::new(
                        match index {
                            0 => "first",
                            _x if _x == fields_count => "last",
                            _ => "_",
                        },
                        variant.span(),
                    )
                })
                .collect();

            quote! {
                #input::#variant(
                    #(#field_match,)*
                ) => {
                    Some((first.start_position()?, last.end_position()?))
                }
            }
        }
    }
}
