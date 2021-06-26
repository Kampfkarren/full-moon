#![recursion_limit = "128"]

extern crate proc_macro;

mod derive;
mod node;
mod symbols;
mod visit;

use derive::DeriveGenerator;

use proc_macro::TokenStream;

#[proc_macro_derive(Visit, attributes(visit))]
pub fn derive_visit(input: TokenStream) -> TokenStream {
    visit::VisitGenerator::derive(input)
}

#[proc_macro_derive(Node, attributes(node))]
pub fn derive_node(input: TokenStream) -> TokenStream {
    node::NodeGenerator::derive(input)
}

#[proc_macro]
pub fn symbols(input: TokenStream) -> TokenStream {
    symbols::parse(input)
}
