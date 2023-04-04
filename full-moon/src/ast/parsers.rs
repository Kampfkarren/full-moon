use super::{
    parser_structs::{ParserResult, ParserState},
    punctuated::{Pair, Punctuated},
    Expression,
};
use crate::{
    ast,
    tokenizer::{Symbol, Token, TokenReference, TokenType},
};

pub fn parse_block(state: &mut ParserState) -> ParserResult<ast::Block> {
    let mut stmts = Vec::new();

    loop {
        match parse_stmt(state) {
            ParserResult::Value(stmt) => {
                // todo: parse semicolons
                stmts.push((stmt, None));
            }
            ParserResult::NotFound => break,
            ParserResult::LexerMoved => {
                if stmts.is_empty() {
                    return ParserResult::LexerMoved;
                } else {
                    break;
                }
            }
        }
    }

    // todo: parse last stmts
    ParserResult::Value(ast::Block {
        stmts,
        last_stmt: None,
    })
}

fn parse_stmt(state: &mut ParserState) -> ParserResult<ast::Stmt> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Local,
        } => {
            const BAD_TOKEN_ERROR: &str = "expected either a variable name or `function`";

            let local_token = state.consume().unwrap();
            let next_token = match state.current() {
                ParserResult::Value(token) => token,
                ParserResult::NotFound => {
                    state.token_error(local_token, BAD_TOKEN_ERROR);
                    return ParserResult::LexerMoved;
                }
                ParserResult::LexerMoved => return ParserResult::LexerMoved,
            };

            match next_token.token_type() {
                TokenType::Identifier { identifier } => {
                    ParserResult::Value(ast::Stmt::LocalAssignment(
                        try_parser!(expect_local_assignment(state, local_token)).unwrap(),
                    ))
                }

                _ => {
                    state.token_error(next_token.clone(), BAD_TOKEN_ERROR);

                    ParserResult::LexerMoved
                }
            }
        }

        _ => ParserResult::NotFound,
    }
}

fn expect_local_assignment(
    state: &mut ParserState,
    local_token: TokenReference,
) -> ParserResult<ast::LocalAssignment> {
    let names = match parse_name_list(state) {
        ParserResult::Value(names) => names,
        ParserResult::NotFound => {
            unreachable!("expect_local_assignment called without upcoming identifier");
        }
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    let mut local_assignment = ast::LocalAssignment {
        local_token,
        name_list: names_to_tokens(names),
        equal_token: None,
        expr_list: Punctuated::new(),
    };

    if matches!(state.current(), ParserResult::Value(token) if token.is_symbol(Symbol::Equal)) {
        local_assignment.equal_token = Some(state.consume().unwrap());
    } else {
        return ParserResult::Value(local_assignment);
    }

    local_assignment.expr_list = match parse_expression_list(state) {
        ParserResult::Value(expr_list) => expr_list,
        ParserResult::NotFound => {
            state.token_error(
                local_assignment.equal_token.unwrap(),
                "expected an expression",
            );

            return ParserResult::LexerMoved;
        }
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    ParserResult::Value(local_assignment)
}

fn parse_expression(state: &mut ParserState) -> ParserResult<Expression> {
    ParserResult::NotFound
}

struct Name {
    name: TokenReference,
    // todo: this is where a type assignment can go
}

fn names_to_tokens(names: Punctuated<Name>) -> Punctuated<TokenReference> {
    names
        .into_pairs()
        .map(|pair| match pair {
            Pair::Punctuated(name, punct) => Pair::Punctuated(name.name, punct),
            Pair::End(name) => Pair::End(name.name),
        })
        .collect()
}

fn parse_name(state: &mut ParserState) -> ParserResult<Name> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Identifier { identifier } => ParserResult::Value(Name {
            name: state.consume().unwrap(),
        }),

        _ => ParserResult::NotFound,
    }
}

fn one_or_more<T, F: Fn(&mut ParserState) -> ParserResult<T>>(
    state: &mut ParserState,
    parser: F,
    delimiters: &[Symbol],
) -> ParserResult<Punctuated<T>> {
    let mut values = Punctuated::new();

    loop {
        let value = match parser(state) {
            ParserResult::Value(value) => value,
            ParserResult::NotFound | ParserResult::LexerMoved => break,
        };

        let next_token = match state.current() {
            ParserResult::Value(token) => token,
            ParserResult::NotFound | ParserResult::LexerMoved => break,
        };

        let symbol = match next_token.token_type() {
            TokenType::Symbol { symbol } => symbol,
            _ => break,
        };

        if delimiters.contains(symbol) {
            values.push(Pair::Punctuated(value, state.consume().unwrap()));
        } else {
            values.push(Pair::End(value));
            break;
        }
    }

    if values.is_empty() {
        return ParserResult::NotFound;
    }

    ParserResult::Value(values)
}

fn parse_name_list(state: &mut ParserState) -> ParserResult<Punctuated<Name>> {
    one_or_more(state, parse_name, &[Symbol::Comma])
}

fn parse_expression_list(state: &mut ParserState) -> ParserResult<Punctuated<Expression>> {
    one_or_more(state, parse_expression, &[Symbol::Comma])
}
