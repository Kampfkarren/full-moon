use crate::tokenizer::TokenReference;
use std::borrow::Cow;

pub trait Sealed {}

impl<'a, T: Clone + Sealed> Sealed for Cow<'a, T> {}
impl<'a> Sealed for TokenReference<'a> {}
impl<T> Sealed for Box<T> {}
impl<T> Sealed for Option<T> {}
impl<T> Sealed for Vec<T> {}
impl<A, B> Sealed for (A, B) {}
