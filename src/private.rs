use crate::tokenizer::TokenReference;

pub trait Sealed {}

impl<'a> Sealed for TokenReference<'a> {}
impl<T> Sealed for Option<T> {}
impl<T> Sealed for Vec<T> {}
impl<A, B> Sealed for (A, B) {}
