use crate::{
    ast::{Ast, AstError},
    tokenizer::{Token, TokenReference, TokenType, TokenizerError},
    Error,
};
use std::borrow::Cow;

pub trait Sealed {}

impl<T> Sealed for &T {}
impl<T> Sealed for &mut T {}
impl<T: ToOwned> Sealed for Cow<'_, T> {}
impl Sealed for Ast {}
impl Sealed for AstError {}
impl Sealed for Error {}
impl Sealed for Token {}
impl Sealed for TokenizerError {}
impl Sealed for TokenReference {}
impl Sealed for TokenType {}
impl<T> Sealed for Box<T> {}
impl<T> Sealed for Option<T> {}
impl<T> Sealed for Vec<T> {}
impl<A, B> Sealed for (A, B) {}
