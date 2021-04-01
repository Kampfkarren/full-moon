use crate::{
    ast::{Ast, AstError},
    tokenizer::{Spanned, Symbol, TokenType, TokenizerError, TriviaType, WithTrivia},
    Error,
};
use std::borrow::Cow;

pub trait Sealed {}

impl<T> Sealed for &T {}
impl<T> Sealed for &mut T {}
impl<T: ToOwned> Sealed for Cow<'_, T> {}
impl Sealed for Ast<'_> {}
impl Sealed for AstError<'_> {}
impl Sealed for Error<'_> {}
impl<T> Sealed for Spanned<T> {}
impl Sealed for TokenizerError {}
impl<T> Sealed for WithTrivia<'_, T> {}
impl Sealed for TokenType<'_> {}
impl Sealed for TriviaType<'_> {}
impl Sealed for Symbol {}
impl<T> Sealed for Box<T> {}
impl<T> Sealed for Option<T> {}
impl<T> Sealed for Vec<T> {}
impl<A, B> Sealed for (A, B) {}
