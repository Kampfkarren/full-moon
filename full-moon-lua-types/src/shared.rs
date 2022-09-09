use std::iter::FromIterator;

use full_moon::{
    ast::{self, punctuated::Pair},
    tokenizer,
};

use mlua::{MetaMethod, ToLua, UserData};

pub struct ContainedSpan {
    start: TokenReference,
    end: TokenReference,
}

impl ContainedSpan {
    pub fn new(contained_span: &ast::span::ContainedSpan) -> Self {
        let (start, end) = contained_span.tokens();

        ContainedSpan {
            start: TokenReference::new(start),
            end: TokenReference::new(end),
        }
    }
}

pub struct Position(tokenizer::Position);

pub struct Punctuated<T>(ast::punctuated::Punctuated<T>);

impl<T> Punctuated<T> {
    pub fn map_from_punctuated<U, F: FnMut(&U) -> T>(
        punctuated: &ast::punctuated::Punctuated<U>,
        mut map: F,
    ) -> Self {
        Punctuated(ast::punctuated::Punctuated::from_iter(
            punctuated.pairs().map(|pair| match pair {
                Pair::Punctuated(value, punctuation) => {
                    Pair::Punctuated(map(value), punctuation.clone())
                }

                Pair::End(value) => Pair::End(map(value)),
            }),
        ))
    }
}

impl<T> FromIterator<Pair<T>> for Punctuated<T> {
    fn from_iter<I: IntoIterator<Item = Pair<T>>>(iter: I) -> Self {
        Punctuated(ast::punctuated::Punctuated::from_iter(iter))
    }
}

impl<T: Clone + UserData + 'static> Punctuated<T> {
    fn to_table<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Table<'lua>> {
        let table = lua.create_table()?;

        for (i, item) in self.0.iter().enumerate() {
            table.set(i + 1, item.clone().to_lua(lua)?)?;
        }

        Ok(table)
    }
}

impl<T: Clone + UserData + 'static> UserData for Punctuated<T> {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(_fields: &mut F) {}

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Iter, |lua, this, _: ()| {
            Ok((
                lua.globals().get::<_, mlua::Function>("next")?,
                this.to_table(lua)?,
            ))
        });

        methods.add_meta_method(MetaMethod::Len, |_, Punctuated(punctuated), _: ()| {
            Ok(punctuated.len())
        });
    }
}

pub struct TokenType(tokenizer::TokenType);

pub struct Token {
    start_position: Position,
    end_position: Position,
    token_type: TokenType,
}

impl From<&tokenizer::Token> for Token {
    fn from(token: &tokenizer::Token) -> Self {
        Token {
            start_position: Position(token.start_position()),
            end_position: Position(token.end_position()),
            token_type: TokenType(token.token_type().clone()),
        }
    }
}

pub struct TokenReference {
    leading_trivia: Vec<Token>,
    token: Token,
    trailing_trivia: Vec<Token>,
}

impl TokenReference {
    pub fn new(token_reference: &tokenizer::TokenReference) -> Self {
        TokenReference {
            leading_trivia: token_reference.leading_trivia().map(Token::from).collect(),
            token: Token::from(token_reference.token()),
            trailing_trivia: token_reference.trailing_trivia().map(Token::from).collect(),
        }
    }
}
