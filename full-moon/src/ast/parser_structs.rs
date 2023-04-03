use crate::tokenizer::Lexer;

pub struct ParserState {
    errors: Vec<crate::Error>,
    lexer: Lexer,
}

pub enum ParserResult<T> {
    // This doesn't necessarily mean that there were no errors,
    // because this can sometimes be a recovered value.
    Value(T),

    // Couldn't get any sort of value, but the lexer has moved.
    // This should always come with an error.
    // Don't create directly, use `ParserState::unrecoverable_error`.
    LexerMoved,

    NotFound,
}
