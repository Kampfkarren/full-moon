use crate::{
    node::Node as NodeTrait,
    tokenizer::TokenReference,
    visitors::{Visit as VisitTrait, VisitMut as VisitMutTrait},
};
use full_moon_derive::{Node, Visit};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type Pairs<'a, T> = Vec<Pair<'a, T>>;

#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[visit(skip_visit_self)]
pub enum Pair<'a, T: NodeTrait + VisitTrait<'a> + VisitMutTrait<'a>> {
    End(T),
    Punctuated(
        T,
        #[cfg_attr(feature = "serde", serde(borrow))] TokenReference<'a>,
    ),
}

impl<'a, T: NodeTrait + VisitTrait<'a> + VisitMutTrait<'a>> Pair<'a, T> {
    pub fn into_value(self) -> T {
        match self {
            Pair::End(value) => value,
            Pair::Punctuated(value, _) => value,
        }
    }

    pub fn punctuation(&self) -> Option<&TokenReference<'a>> {
        match self {
            Pair::End(_) => None,
            Pair::Punctuated(_, punctuation) => Some(punctuation),
        }
    }
}
