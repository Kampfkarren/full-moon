use crate::derive::*;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

/// `prefix` is `self.` for a struct, `*` for an enum, whose `ref` fields need reborrowing.
fn token_getter(ty: &syn::Type, ident: &syn::Ident, prefix: &TokenStream) -> TokenStream {
    let field = quote! { &#prefix #ident };
    let builder = builder_ident();

    match type_name(ty) {
        Some(name) if name == "ContainedSpan" => quote! { #builder.push_open(#field); },
        Some(name) if name == "TokenReference" => quote! { #builder.push_token(#field); },
        _ => quote! { #builder.push(#field); },
    }
}

/// A named variant's fields become locals, so this must be a name no field takes:
/// called `builder`, it would shadow that field and `&*builder` would not compile.
fn builder_ident() -> Ident {
    format_ident!("__full_moon_builder")
}

/// The last path segment, so `span::ContainedSpan` gives `ContainedSpan`.
/// Nothing is unwrapped, so `Option<ContainedSpan>` gives `Option`.
fn type_name(ty: &syn::Type) -> Option<&syn::Ident> {
    match ty {
        syn::Type::Path(path) => path.path.segments.last().map(|segment| &segment.ident),
        _ => None,
    }
}

#[derive(PartialEq)]
enum NodeHint {
    FullRange,
    /// Emits the innermost pending `ContainedSpan` close after this field, not at the end.
    Close,
    /// Emits this field right after the named one, where `Display` puts it.
    After(String),
    /// Emits element `i` of each named `Vec<Option<_>>` before its comma.
    Interleave(Vec<String>),
}

impl Hint for NodeHint {
    fn unit(name: String) -> Option<Self> {
        match name.as_str() {
            "full_range" => Some(NodeHint::FullRange),
            "close" => Some(NodeHint::Close),
            _ => None,
        }
    }

    fn key_value(key: String, value: String) -> Option<Self> {
        match key.as_str() {
            "after" => Some(NodeHint::After(value)),
            "interleave" => Some(NodeHint::Interleave(
                value
                    .split(',')
                    .map(|name| name.trim().to_owned())
                    .collect(),
            )),
            _ => None,
        }
    }
}

pub struct NodeGenerator;

impl DeriveGenerator for NodeGenerator {
    fn complete(input: &syn::DeriveInput, tokens: TokenStream) -> TokenStream {
        let input_ident = &input.ident;
        let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

        let macro_name = format_ident!("NodeGenerator{}", input_ident);

        let pattern = quote! {{
            range => $range:expr,
            similar => $similar:expr,
            tokens => $tokens:expr,
        }};

        quote! {
            macro_rules! #macro_name {
                ("range", #pattern) => {
                    $range
                };

                ("similar", #pattern) => {
                    $similar
                };

                ("tokens", #pattern) => {
                    $tokens
                }
            }

            impl #impl_generics crate::node::Node #impl_generics for #input_ident #ty_generics #where_clause {
                fn start_position(&self) -> Option<crate::tokenizer::Position> {
                    Some(#macro_name!("range", { #tokens })?.0)
                }

                fn end_position(&self) -> Option<crate::tokenizer::Position> {
                    Some(#macro_name!("range", { #tokens })?.1)
                }

                fn similar(&self, other: &Self) -> bool {
                    #macro_name!("similar", { #tokens })
                }

                fn tokens<'a>(&'a self) -> crate::node::Tokens<'a> {
                    #macro_name!("tokens", { #tokens })
                }
            }

            impl #impl_generics crate::private::Sealed for #input_ident #ty_generics #where_clause {}
        }
    }
}

impl StructGenerator for NodeGenerator {
    fn generate(ident: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let range = StructRangeGenerator::generate(ident, strukt);
        let similar = StructSimilarGenerator::generate(ident, strukt);
        let tokens = StructTokensGenerator::generate(ident, strukt);

        quote! {
            range => { #range },
            similar => { #similar },
            tokens => { #tokens },
        }
    }
}

pub struct StructRangeGenerator;

impl StructGenerator for StructRangeGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let fields = strukt
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        let full_range = strukt.fields.iter().find(|field| {
            search_hints::<NodeHint>("node", &field.attrs).any(|hint| hint == NodeHint::FullRange)
        });

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
                Some((None #(#start_position)*?, None #(#end_position)*?))
            }
        }
    }
}

pub struct StructSimilarGenerator;

impl StructGenerator for StructSimilarGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        let fields = strukt
            .fields
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        quote! {
            #(
                self.#fields.similar(&other.#fields) &&
            )* true
        }
    }
}

/// Emits an `interleave` hint: element `i` of each named `Vec` follows element `i`.
fn interleave_statement(
    ident: &syn::Ident,
    interleaved_with: &[Ident],
    prefix: &TokenStream,
) -> TokenStream {
    let list = quote! { &#prefix #ident };
    let builder = builder_ident();

    match interleaved_with {
        [one] => quote! { #builder.push_punctuated_with(#list, &#prefix #one); },

        [first, second] => {
            quote! { #builder.push_punctuated_with_two(#list, &#prefix #first, &#prefix #second); }
        }

        _ => unreachable!("`build_tokens_body` rejects other lengths"),
    }
}

#[derive(Default)]
struct FieldHints {
    after: Option<Ident>,
    close: bool,
    /// Narrowed to the fields this build has.
    interleave: Vec<Ident>,
}

impl FieldHints {
    /// A name matching no field is dropped, since `#[cfg]` left it out of this build.
    ///
    /// A typo looks the same: rustc strips those fields before the derive sees them.
    /// `LocalAssignment` relies on this, naming both `attributes` and `type_specifiers`.
    fn of(field: &syn::Field, fields: &[&Ident]) -> Result<Self, syn::Error> {
        let mut hints = Self::default();

        for hint in search_hints("node", &field.attrs) {
            match hint {
                NodeHint::FullRange => {}
                NodeHint::Close => hints.close = true,
                NodeHint::After(name) => hints.after = resolve_field_name(field, &name, fields)?,
                NodeHint::Interleave(names) => {
                    for name in names {
                        let Some(ident) = resolve_field_name(field, &name, fields)? else {
                            continue;
                        };

                        // Once, so `build_tokens_body`'s count of interleaved fields holds.
                        if !hints.interleave.contains(&ident) {
                            hints.interleave.push(ident);
                        }
                    }
                }
            }
        }

        Ok(hints)
    }
}

/// `None` means it parses but matches no field here. An unparseable one is an error.
fn resolve_field_name(
    field: &syn::Field,
    name: &str,
    fields: &[&Ident],
) -> Result<Option<Ident>, syn::Error> {
    let ident = syn::parse_str::<Ident>(name)
        .map_err(|_| syn::Error::new_spanned(field, format!("`{name}` is not a field name")))?;

    Ok(Some(ident).filter(|ident| fields.contains(&ident)))
}

fn field_statement(field: &syn::Field, interleave: &[Ident], prefix: &TokenStream) -> TokenStream {
    let ident = field.ident.as_ref().expect("named field");

    match interleave {
        [] => token_getter(&field.ty, ident, prefix),
        names => interleave_statement(ident, names, prefix),
    }
}

/// Emits a field, then whatever `after` hints put behind it.
///
/// `as_follower` means another field named us in its `after`, so our tokens belong there.
/// `emitting` is the chain being walked, so a cycle stops instead of recursing forever;
/// `build_tokens_body` reports it, since from the top it never happens.
fn emit_field(
    index: usize,
    fields: &[&syn::Field],
    hints: &[FieldHints],
    prefix: &TokenStream,
    as_follower: bool,
    emitting: &mut Vec<Ident>,
    order: &mut Vec<Ident>,
) -> TokenStream {
    let field = fields[index];
    let ident = field.ident.as_ref().expect("named field");
    let hint = &hints[index];

    if emitting.contains(ident) {
        return quote! {};
    }

    order.push(ident.clone());

    let mut statement = if as_follower || hint.after.is_none() {
        field_statement(field, &hint.interleave, prefix)
    } else {
        quote! {}
    };

    if hint.close {
        let builder = builder_ident();
        statement.extend(quote! { #builder.close(); });
    }

    emitting.push(ident.clone());

    // The follower may be declared before this field, so the whole list is searched.
    for follower in 0..fields.len() {
        if hints[follower].after.as_ref() == Some(ident) {
            statement.extend(emit_field(
                follower, fields, hints, prefix, true, emitting, order,
            ));
        }
    }

    emitting.pop();

    statement
}

/// Checks that each `#[node(close)]` has a `ContainedSpan` still open when it is emitted.
///
/// Walks `order`: the emission order is what `TokenBuilder` sees, not the declaration one.
/// A `close` with nothing to close would fail silently, so it is reported on its field.
fn close_matches_spans(
    fields: &[&syn::Field],
    idents: &[&Ident],
    hints: &[FieldHints],
    order: &[Ident],
) -> Result<(), syn::Error> {
    let mut open_spans = 0;

    for ident in order {
        let index = idents
            .iter()
            .position(|candidate| *candidate == ident)
            .expect("emitted field is in the list");

        if type_name(&fields[index].ty).is_some_and(|name| name == "ContainedSpan") {
            open_spans += 1;
        }

        if hints[index].close {
            if open_spans == 0 {
                return Err(syn::Error::new_spanned(
                    fields[index],
                    "`#[node(close)]` on a field emitted before any `ContainedSpan` opens",
                ));
            }

            open_spans -= 1;
        }
    }

    Ok(())
}

/// Builds the `Node::tokens` body: declaration order, except where `Display` differs.
///
/// `prefix` reaches each field: `self.` for a struct, `*` for a destructured enum.
fn tokens_body(fields: &[&syn::Field], prefix: &TokenStream) -> TokenStream {
    match build_tokens_body(fields, prefix) {
        Err(error) => error.to_compile_error(),
        Ok(body) => body,
    }
}

/// [`tokens_body`], with the annotation checks reported as errors.
fn build_tokens_body(
    fields: &[&syn::Field],
    prefix: &TokenStream,
) -> Result<TokenStream, syn::Error> {
    let idents = fields
        .iter()
        .map(|field| field.ident.as_ref().expect("named field"))
        .collect::<Vec<_>>();

    let hints = fields
        .iter()
        .map(|field| FieldHints::of(field, &idents))
        .collect::<Result<Vec<_>, _>>()?;

    // The interleaving field's statement emits these, so they get no statement.
    let mut interleaved = Vec::new();
    for ident in hints.iter().flat_map(|hint| &hint.interleave) {
        if !interleaved.contains(&ident) {
            interleaved.push(ident);
        }
    }

    // Written one element at a time, so `close` and `after` have nothing to attach to.
    for (index, ident) in idents.iter().enumerate() {
        if hints[index].interleave.len() > 2 {
            return Err(syn::Error::new_spanned(
                fields[index],
                "`#[node(interleave = ...)]` takes one or two field names",
            ));
        }

        if interleaved.contains(ident) {
            let hint = &hints[index];
            if hint.close {
                return Err(syn::Error::new_spanned(
                    fields[index],
                    "`#[node(close)]` on a field that another field interleaves",
                ));
            }

            if hint.after.is_some() {
                return Err(syn::Error::new_spanned(
                    fields[index],
                    "`#[node(after)]` on a field that another field interleaves",
                ));
            }

            if let Some(other) = fields
                .iter()
                .zip(&hints)
                .find(|(_, other)| other.after.as_ref() == Some(*ident))
            {
                return Err(syn::Error::new_spanned(
                    other.0,
                    "`#[node(after = ...)]` names a field that another field interleaves",
                ));
            }
        }
    }

    let mut statements = Vec::new();
    let mut emitting = Vec::new();
    let mut order = Vec::new();

    for (index, ident) in idents.iter().enumerate() {
        // Emitted by another field's statement.
        if hints[index].after.is_some() || interleaved.contains(ident) {
            continue;
        }

        statements.push(emit_field(
            index,
            fields,
            &hints,
            prefix,
            false,
            &mut emitting,
            &mut order,
        ));
    }

    // A field on a cycle is emitted nowhere. Report it against the first such field.
    if order.len() != fields.len().saturating_sub(interleaved.len()) {
        let missing = fields
            .iter()
            .zip(&idents)
            .find(|(_, ident)| !order.contains(ident))
            .map_or_else(
                || {
                    syn::Error::new(
                        proc_macro2::Span::call_site(),
                        "`#[node(after = ...)]` forms a cycle",
                    )
                },
                |(field, _)| syn::Error::new_spanned(field, "`#[node(after = ...)]` forms a cycle"),
            );

        return Err(missing);
    }

    close_matches_spans(fields, &idents, &hints, &order)?;

    let builder = builder_ident();

    Ok(quote! {
        let mut #builder = crate::node::TokenBuilder::default();
        #(#statements)*
        #builder.build()
    })
}

pub struct StructTokensGenerator;

impl StructGenerator for StructTokensGenerator {
    fn generate(_: &syn::Ident, strukt: &syn::DataStruct) -> TokenStream {
        tokens_body(&strukt.fields.iter().collect::<Vec<_>>(), &quote! { self. })
    }
}

impl EnumGenerator for NodeGenerator {
    fn generate(ident: &syn::Ident, enumm: &syn::DataEnum) -> TokenStream {
        let range = EnumRangeGenerator::generate(ident, enumm);
        let similar = EnumSimilarGenerator::generate(ident, enumm);
        let tokens = EnumTokensGenerator::generate(ident, enumm);

        quote! {
            range => {
                #[allow(unused)]
                #range
            },

            similar => { #similar },
            tokens => { #tokens },
        }
    }
}

pub struct EnumRangeGenerator;

impl MatchEnumGenerator for EnumRangeGenerator {
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

        let full_range = named.named.iter().find(|field| {
            search_hints::<NodeHint>("node", &field.attrs).any(|hint| hint == NodeHint::FullRange)
        });

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
                Some((None #(#start_position)*?, None #(#end_position)*?))
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

pub struct EnumSimilarGenerator;

impl MatchEnumGenerator for EnumSimilarGenerator {
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

        let other_fields: Vec<_> = fields
            .iter()
            .map(|ident| format_ident!("other_{}", ident))
            .collect();

        quote! {
            #input::#variant {
                #(#fields,)*
            } => {
                if let #input::#variant {
                    #(
                        #fields: #other_fields,
                    )*
                } = &other {
                    #(
                        #fields.similar(#other_fields) &&
                    )* true
                } else {
                    false
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

        let other_fields: Vec<_> = fields
            .iter()
            .map(|ident| format_ident!("other_{}", ident))
            .collect();

        quote! {
            #input::#variant(
                #(#fields,)*
            ) => {
                if let #input::#variant(#(#other_fields,)*) = &other {
                    #(
                        #fields.similar(#other_fields) &&
                    )* true
                } else {
                    false
                }
            }
        }
    }

    fn case_unit(input: &syn::Ident, variant: &syn::Ident) -> TokenStream {
        quote! {
            #input::#variant => other == #input::#variant
        }
    }
}

pub struct EnumTokensGenerator;

impl MatchEnumGenerator for EnumTokensGenerator {
    const DEREF: bool = true;

    fn case_named(
        input: &syn::Ident,
        variant: &syn::Ident,
        named: &syn::FieldsNamed,
    ) -> TokenStream {
        let named = &named.named;

        let fields = named
            .iter()
            .map(|field| field.ident.as_ref().unwrap())
            .collect::<Vec<_>>();

        let body = tokens_body(&named.iter().collect::<Vec<_>>(), &quote! { * });

        quote! {
            #input::#variant {
                #(ref #fields,)*
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
        let names: Vec<_> = fields
            .unnamed
            .iter()
            .enumerate()
            .map(|(index, _)| format_ident!("__self_{}", index))
            .collect();

        // Unnamed fields have no names to reference, so they go in declaration order.
        let mut getters = Vec::with_capacity(fields.unnamed.len());

        for (field, name) in fields.unnamed.iter().zip(&names) {
            getters.push(token_getter(&field.ty, name, &quote! { * }));
        }

        let builder = builder_ident();

        quote! {
            #input::#variant(
                #(ref #names,)*
             ) => {
                let mut #builder = crate::node::TokenBuilder::default();
                #(#getters)*
                #builder.build()
            }
        }
    }
}
