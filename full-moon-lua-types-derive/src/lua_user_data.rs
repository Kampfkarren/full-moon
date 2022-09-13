use proc_macro::TokenStream;
use quote::{format_ident, quote};

fn match_all<F: Fn(&syn::Ident) -> proc_macro2::TokenStream>(
    input_ident: &syn::Ident,
    variants: &syn::punctuated::Punctuated<syn::Variant, syn::token::Comma>,
    case: F,
) -> proc_macro2::TokenStream {
    let mut cases = Vec::new();

    for variant in variants {
        let ident = &variant.ident;
        let result = case(ident);

        match variant.fields {
            syn::Fields::Named(_) => {
                cases.push(quote! {
                    #input_ident::#ident { .. } => { #result }
                });
            }

            syn::Fields::Unnamed(_) => {
                cases.push(quote! {
                    #input_ident::#ident(..) => { #result }
                });
            }

            syn::Fields::Unit => {
                cases.push(quote! {
                    #input_ident::#ident => { #result }
                });
            }
        }
    }

    quote! {
        match this {
            #(#cases,)*
        }
    }
}

pub fn derive(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);

    let input_enum = match &input.data {
        syn::Data::Enum(input_enum) => input_enum,
        _ => panic!("can only derive for enums"),
    };

    let input_ident = &input.ident;

    let match_kind = match_all(input_ident, &input_enum.variants, |variant| {
        quote! {
            stringify!(#variant)
        }
    });

    let match_to_string = match_all(input_ident, &input_enum.variants, |variant| {
        quote! {
            format!("{}::{}", stringify!(#input_ident), stringify!(#variant))
        }
    });

    let match_match = {
        let mut cases = Vec::with_capacity(input_enum.variants.len());

        for variant in &input_enum.variants {
            let variant_ident = &variant.ident;

            match &variant.fields {
                syn::Fields::Named(fields) => {
                    let fields = fields
                        .named
                        .iter()
                        .map(|field| &field.ident)
                        .collect::<Vec<_>>();

                    cases.push(quote! {
                        #input_ident::#variant_ident { #(#fields),* } => {
                            let mut table = lua.create_table()?;

                            #(
                                table.set(stringify!(#fields), #fields.prepare_for_lua(lua)?)?;
                            )*

                            (stringify!(#variant_ident), table.to_lua_multi(lua)?)
                        }
                    });
                }

                syn::Fields::Unnamed(fields) => {
                    let fields = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(index, _)| format_ident!("_{index}"))
                        .collect::<Vec<_>>();

                    cases.push(quote! {
                        #input_ident::#variant_ident(#(#fields),*) => {
                            let mut fields = Vec::new();

                            #(
                                fields.push(#fields.prepare_for_lua(lua)?);
                            )*

                            (stringify!(#variant_ident), mlua::MultiValue::from_vec(fields))
                        }
                    });
                }

                syn::Fields::Unit => {
                    cases.push(quote! {
                        #input_ident::#variant_ident => {
                            (stringify!(#variant_ident), ().to_lua_multi(lua)?)
                        }
                    });
                }
            }
        }

        quote! {
            use mlua::ToLuaMulti;

            let (function_name, args) = match this {
                #(#cases,)*
            };

            match table.get::<_, Option<mlua::Value>>(function_name)? {
                Some(mlua::Value::Function(function)) => {
                    function.call::<_, mlua::Value>(args)
                }

                Some(other) => {
                    Err(mlua::Error::external(format!(
                        "expected function for {}, got {}",
                        function_name,
                        other.type_name(),
                    )))
                }

                None => {
                    Ok(mlua::Value::Nil)
                }
            }
        }
    };

    let match_create_ast_node = {
        let mut cases = Vec::with_capacity(input_enum.variants.len());

        for variant in &input_enum.variants {
            let variant_ident = &variant.ident;

            if variant_ident == "NonStandard" {
                let fields = match &variant.fields {
                    syn::Fields::Named(_) => quote! { { .. } },
                    syn::Fields::Unnamed(_) => quote! { ( .. ) },
                    syn::Fields::Unit => quote! {},
                };

                cases.push(quote! {
                    #input_ident::#variant_ident #fields => {
                        None
                    }
                });

                continue;
            }

            match &variant.fields {
                syn::Fields::Named(field) => {
                    let fields = field
                        .named
                        .iter()
                        .map(|field| &field.ident)
                        .collect::<Vec<_>>();

                    cases.push(quote! {
                        #input_ident::#variant_ident { #(#fields),* } => {
                            Some(full_moon::ast::#input_ident::#variant_ident {
                                #(#fields: #fields.create_ast_node()?),*
                            })
                        }
                    });
                }

                syn::Fields::Unnamed(field) => {
                    let fields = field
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(index, _)| format_ident!("_{index}"))
                        .collect::<Vec<_>>();

                    let body = match variant
                        .attrs
                        .iter()
                        .filter_map(|attr| {
                            if !attr.path.is_ident("lua") {
                                return None;
                            }

                            let name_value = attr
                                .parse_args::<syn::MetaNameValue>()
                                .expect("expected name value for #[lua(create_ast_node)]");

                            if name_value.path.is_ident("create_ast_node") {
                                Some(match name_value.lit {
                                    syn::Lit::Str(lit_str) => lit_str.value(),
                                    _ => panic!("expected string for #[lua(create_ast_node)]"),
                                })
                            } else {
                                None
                            }
                        })
                        .next()
                    {
                        Some(create_ast_node_attr) => {
                            syn::parse_str(&create_ast_node_attr).expect("expected valid rust code")
                        }

                        None => {
                            quote! {
                                full_moon::ast::#input_ident::#variant_ident(
                                    #(#fields.create_ast_node()?),*
                                )
                            }
                        }
                    };

                    cases.push(quote! {
                        #input_ident::#variant_ident(#(#fields),*) => {
                            #body.into()
                        }
                    });
                }

                syn::Fields::Unit => {
                    cases.push(quote! {
                        #input_ident::#variant_ident => {
                            Some(full_moon::ast::#input_ident::#variant_ident)
                        }
                    });
                }
            }
        }

        quote! {
            match self {
                #(#cases,)*
            }
        }
    };

    quote! {
        impl mlua::UserData for #input_ident {
			fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
				fields.add_field_method_get("kind", |_, this| {
					Ok(#match_kind)
				});
			}

            fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        		crate::mlua_util::add_core_metamethods_no_tostring(stringify!(#input_ident), methods);
                crate::mlua_util::add_print(methods);
                crate::mlua_util::add_visit(methods);

        		methods.add_meta_method(mlua::MetaMethod::ToString, |_, this, _: ()| {
        			Ok(#match_to_string)
        		});

				methods.add_method("match", |lua, this, table: mlua::Table| {
					#match_match
				});
            }
        }

        impl crate::create_ast_node::CreateAstNode for #input_ident {
            type Node = full_moon::ast::#input_ident;

            fn create_ast_node(&self) -> Option<Self::Node> {
                #match_create_ast_node
            }
        }
    }
    .into()
}
