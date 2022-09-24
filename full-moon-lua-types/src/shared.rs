use std::iter::FromIterator;

use full_moon::{
    ast::{self, punctuated::Pair},
    node::Node,
    tokenizer,
};

use mlua::{MetaMethod, ToLua, UserData};

use crate::{
    ast_traits::CreateAstNode,
    mlua_util::{add_core_meta_methods, add_create_ast_node_methods, add_print},
    AstToLua,
};

#[derive(Clone)]
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

impl UserData for ContainedSpan {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("ContainedSpan", methods);
        add_create_ast_node_methods(methods);
    }
}

impl CreateAstNode for ContainedSpan {
    type Node = ast::span::ContainedSpan;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(ast::span::ContainedSpan::new(
            self.start.create_ast_node()?,
            self.end.create_ast_node()?,
        ))
    }
}

impl AstToLua for ast::span::ContainedSpan {
    fn ast_to_lua<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Value<'lua>> {
        ContainedSpan::new(self).to_lua(lua)
    }
}

#[derive(Clone, Copy)]
pub struct Position(tokenizer::Position);

#[derive(Clone)]
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

impl<T: Clone + UserData + Send + Sync + 'static> Punctuated<T> {
    fn values<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Table<'lua>> {
        let table = lua.create_table()?;

        for (i, item) in self.0.iter().enumerate() {
            table.set(i + 1, item.clone().to_lua(lua)?)?;
        }

        Ok(table)
    }
}

impl<T> FromIterator<Pair<T>> for Punctuated<T> {
    fn from_iter<I: IntoIterator<Item = Pair<T>>>(iter: I) -> Self {
        Punctuated(ast::punctuated::Punctuated::from_iter(iter))
    }
}

impl<N: Node, T: Clone + UserData + CreateAstNode<Node = N> + Send + Sync + 'static> UserData
    for Punctuated<T>
{
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(_fields: &mut F) {}

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(MetaMethod::Iter, |lua, this, _: ()| {
            Ok((
                lua.globals().get::<_, mlua::Function>("next")?,
                this.values(lua)?,
            ))
        });

        methods.add_meta_method(MetaMethod::Len, |_, Punctuated(punctuated), _: ()| {
            Ok(punctuated.len())
        });

        methods.add_method("values", |lua, this, _: ()| {
            this.values(lua).map_err(mlua::Error::external)
        });

        add_create_ast_node_methods(methods);
    }
}

impl<T: CreateAstNode> CreateAstNode for Punctuated<T> {
    type Node = ast::punctuated::Punctuated<T::Node>;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(ast::punctuated::Punctuated::from_iter(
            self.0
                .pairs()
                .map(|pair| {
                    Some(match pair {
                        Pair::Punctuated(value, punctuation) => {
                            Pair::Punctuated(value.create_ast_node()?, punctuation.clone())
                        }

                        Pair::End(value) => Pair::End(value.create_ast_node()?),
                    })
                })
                .collect::<Option<Vec<_>>>()?,
        ))
    }
}

#[derive(Clone)]
pub struct TokenType(tokenizer::TokenType);

#[derive(Clone)]
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

impl CreateAstNode for Token {
    type Node = tokenizer::Token;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            tokenizer::Token::new(self.token_type.0.clone())
                .with_start_position(self.start_position.0)
                .with_end_position(self.end_position.0),
        )
    }
}

impl UserData for Token {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Token", methods);
        add_print(methods);
    }
}

#[derive(Clone)]
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

impl UserData for TokenReference {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("token", |lua, this: &Self| this.token.clone().to_lua(lua));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("TokenReference", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
    }
}

impl CreateAstNode for TokenReference {
    type Node = tokenizer::TokenReference;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(tokenizer::TokenReference::new(
            self.leading_trivia
                .iter()
                .map(Token::create_ast_node)
                .collect::<Option<Vec<_>>>()?,
            self.token.create_ast_node()?,
            self.trailing_trivia
                .iter()
                .map(Token::create_ast_node)
                .collect::<Option<Vec<_>>>()?,
        ))
    }
}

impl AstToLua for tokenizer::TokenReference {
    fn ast_to_lua<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Value<'lua>> {
        TokenReference::new(self).to_lua(lua)
    }
}
