use crate::{
    ast::{Ast, AstError},
    tokenizer::{Token, TokenReference, TokenType, TokenizerError},
    Error,
};
use std::borrow::Cow;

pub trait Sealed {}

impl<T> Sealed for &T {}
impl<T> Sealed for &mut T {}
impl<T: Clone> Sealed for Cow<'_, T> {}
impl Sealed for Ast<'_> {}
impl Sealed for AstError<'_> {}
impl Sealed for Error<'_> {}
impl Sealed for Token<'_> {}
impl Sealed for TokenizerError {}
impl Sealed for TokenReference<'_> {}
impl Sealed for TokenType<'_> {}
impl<T> Sealed for Box<T> {}
impl<T> Sealed for Option<T> {}
impl<T> Sealed for Vec<T> {}
impl<A, B> Sealed for (A, B) {}
