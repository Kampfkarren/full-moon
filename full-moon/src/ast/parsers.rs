// rewrite todo: do a lot of tests for reasonable cases of recoverability (like local x = \n local y = 1)
// rewrite todo: test function() true end, i think it's not going to highlight "true" and i want it to.
// actually, it might but only because the last token isn't an eof. parser is going to have to throw out those tokens
// and then read another block and merge them or something? wow that sounds awful because it won't work inside if's. good luck
// maybe keep parsing until there's an `end`???? but then global blocks...aaa

use super::{
    parser_structs::{ParserResult, ParserState},
    punctuated::{Pair, Punctuated},
    span::ContainedSpan,
    Expression, FunctionBody, Parameter,
};
use crate::{
    ast, // rewrite todo: make everything use this
    node::Node,
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

// Blocks in general are not very fallible. This means, for instance, not finishing `function()`
// will result in a completely ignored function body.
// This is an opinionated choice because I believe selene is going to produce terrible outputs if we don't.
// rewrite todo: I don't love that this only accepts one token. it should be range(s?)
fn parse_block_with_end(
    state: &mut ParserState,
    start_for_errors: &TokenReference,
) -> Result<(ast::Block, TokenReference), ()> {
    let block = match parse_block(state) {
        ParserResult::Value(block) => block,
        ParserResult::NotFound => unreachable!("parse_block should always return a value"),
        ParserResult::LexerMoved => return Err(()),
    };

    let end_token = match state.current() {
        ParserResult::Value(token) if token.is_symbol(Symbol::End) => state.consume().unwrap(),

        ParserResult::Value(_) | ParserResult::NotFound => {
            state.token_error(start_for_errors.clone(), "expected `end` to close block");
            return Err(());
        }

        ParserResult::LexerMoved => return Err(()),
    };

    Ok((block, end_token))
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

        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        }
        | TokenType::Identifier { .. } => {
            // unwrap() because we're always starting on the right path
            // rewrite todo: i'm very skeptical of the fallibility of this
            let (prefix, suffixes) = try_parser!(parse_prefix_and_suffixes(state)).unwrap();

            match suffixes.last().expect("suffixes should never be empty") {
                ast::Suffix::Call(call) => {
                    ParserResult::Value(ast::Stmt::FunctionCall(ast::FunctionCall {
                        prefix,
                        suffixes,
                    }))
                }

                ast::Suffix::Index(_) => todo!(),
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

    match parse_expression_list(state) {
        ParserResult::Value(expr_list) => local_assignment.expr_list = expr_list,

        ParserResult::NotFound => {
            state.token_error(
                // rewrite todo: This is a great reason why we should be using ranges and not Position
                local_assignment.equal_token.clone().unwrap(),
                "expected an expression",
            );
        }

        ParserResult::LexerMoved => {}
    };

    ParserResult::Value(local_assignment)
}

fn parse_prefix(state: &mut ParserState) -> ParserResult<ast::Prefix> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        } => {
            todo!()
        }

        TokenType::Identifier { identifier } => {
            ParserResult::Value(ast::Prefix::Name(state.consume().unwrap()))
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_arguments(state: &mut ParserState) -> ParserResult<ast::FunctionArgs> {
    let current = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current.token_type() {
        TokenType::Symbol {
            symbol: Symbol::LeftParen,
        } => {
            let left_parenthesis = state.consume().unwrap();
            let arguments = try_parser!(parse_expression_list(state)).unwrap_or_default();
            let right_parenthesis = match state.current() {
                ParserResult::Value(token) if token.is_symbol(Symbol::RightParen) => {
                    state.consume().unwrap()
                }

                ParserResult::LexerMoved => return ParserResult::LexerMoved,

                _ => {
                    state.token_error(
                        left_parenthesis.clone(),
                        "expected `)` to close function call",
                    );

                    TokenReference::symbol(")").unwrap()
                }
            };

            ParserResult::Value(ast::FunctionArgs::Parentheses {
                parentheses: ContainedSpan::new(left_parenthesis, right_parenthesis),
                arguments,
            })
        }

        TokenType::Symbol {
            symbol: Symbol::LeftBrace,
        } => {
            todo!("table call")
        }

        TokenType::StringLiteral { .. } => {
            todo!("string call")
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_suffix(state: &mut ParserState) -> ParserResult<ast::Suffix> {
    let current = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Dot,
        } => {
            todo!("dot index");
        }

        TokenType::Symbol {
            symbol: Symbol::LeftBracket,
        } => {
            todo!("bracket index");
        }

        TokenType::Symbol {
            symbol: Symbol::LeftParen | Symbol::LeftBrace,
        }
        | TokenType::StringLiteral { .. } => {
            let arguments = try_parser!(parse_arguments(state)).unwrap();
            ParserResult::Value(ast::Suffix::Call(ast::Call::AnonymousCall(arguments)))
        }

        _ => ParserResult::NotFound,
    }
}

fn parse_prefix_and_suffixes(
    state: &mut ParserState,
) -> ParserResult<(ast::Prefix, Vec<ast::Suffix>)> {
    let prefix = match parse_prefix(state) {
        ParserResult::Value(prefix) => prefix,
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
        ParserResult::NotFound => return ParserResult::NotFound,
    };

    let mut suffixes = Vec::new();

    loop {
        match parse_suffix(state) {
            ParserResult::Value(suffix) => {
                suffixes.push(suffix);
            }

            ParserResult::LexerMoved => {
                if suffixes.is_empty() {
                    return ParserResult::LexerMoved;
                } else {
                    break;
                }
            }

            ParserResult::NotFound => {
                if suffixes.is_empty() {
                    state.token_error(
                        match state.current() {
                            ParserResult::Value(token) => token.clone(),
                            ParserResult::NotFound => prefix.tokens().last().unwrap().clone(),
                            ParserResult::LexerMoved => unreachable!(),
                        },
                        "expected either a call or an index",
                    );

                    return ParserResult::LexerMoved;
                } else {
                    break;
                }
            }
        };
    }

    ParserResult::Value((prefix, suffixes))
}

fn parse_expression(state: &mut ParserState) -> ParserResult<Expression> {
    let primary_expression = match parse_primary_expression(state) {
        ParserResult::Value(expression) => expression,
        ParserResult::NotFound => return ParserResult::NotFound,
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    parse_expression_with_precedence(state, primary_expression, 0)
}

fn parse_primary_expression(state: &mut ParserState) -> ParserResult<Expression> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Function,
        } => {
            let function_token = state.consume().unwrap();
            let function_body = match parse_function_body(state) {
                ParserResult::Value(body) => body,
                ParserResult::LexerMoved => return ParserResult::LexerMoved,
                ParserResult::NotFound => {
                    state.token_error(function_token, "expected a function body");
                    return ParserResult::LexerMoved;
                }
            };

            ParserResult::Value(Expression::Function((function_token, function_body)))
        }

        TokenType::StringLiteral { .. } => {
            let string_token = state.consume().unwrap();
            ParserResult::Value(Expression::String(string_token))
        }

        TokenType::Number { .. } => {
            let number_token = state.consume().unwrap();
            ParserResult::Value(Expression::Number(number_token))
        }

        _ => ParserResult::NotFound,
    }
}

// rewrite todo: i think this should be iterative instead of recursive
fn parse_expression_with_precedence(
    state: &mut ParserState,
    mut lhs: Expression,
    precedence: u8,
) -> ParserResult<Expression> {
    ParserResult::Value(lhs)
}

fn parse_function_body(state: &mut ParserState) -> ParserResult<FunctionBody> {
    let left_parenthesis = match try_parser!(state.current()) {
        Some(token) if token.is_symbol(Symbol::LeftParen) => state.consume().unwrap(),
        _ => return ParserResult::NotFound,
    };

    let parameters = match one_or_more(state, parse_parameter, &[Symbol::Comma]) {
        ParserResult::Value(parameters) => parameters,
        ParserResult::NotFound => Punctuated::new(),
        // rewrite todo: i want to support function x(a, b, c,) as being fallible
        ParserResult::LexerMoved => return ParserResult::LexerMoved,
    };

    let right_parenthesis = match try_parser!(state.current()) {
        Some(token) if token.is_symbol(Symbol::RightParen) => state.consume().unwrap(),
        _ => {
            state.token_error(left_parenthesis, "expected a `)`");
            return ParserResult::LexerMoved;
        }
    };

    let (block, end) = match parse_block_with_end(state, &right_parenthesis) {
        Ok((block, end)) => (block, end),
        Err(()) => return ParserResult::LexerMoved,
    };

    ParserResult::Value(FunctionBody {
        parameters_parentheses: ContainedSpan::new(left_parenthesis, right_parenthesis),
        parameters: parameters
            .into_pairs()
            .map(|parameter| parameter.map(|parameter| parameter.parameter))
            .collect(),

        block,
        end_token: end,
    })
}

struct PackedParameter {
    parameter: Parameter,
    // rewrite todo: this is where a type assignment can go
}

fn parse_parameter(state: &mut ParserState) -> ParserResult<PackedParameter> {
    let current_token = match try_parser!(state.current()) {
        Some(token) => token,
        None => return ParserResult::NotFound,
    };

    match current_token.token_type() {
        TokenType::Symbol {
            symbol: Symbol::Ellipse,
        } => ParserResult::Value(PackedParameter {
            parameter: Parameter::Ellipse(state.consume().unwrap()),
        }),

        TokenType::Identifier { identifier } => ParserResult::Value(PackedParameter {
            parameter: Parameter::Name(state.consume().unwrap()),
        }),

        _ => ParserResult::NotFound,
    }
}

struct Name {
    name: TokenReference,
    // rewrite todo: this is where a type assignment can go
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

        if let TokenType::Symbol { symbol } = next_token.token_type() {
            if delimiters.contains(symbol) {
                values.push(Pair::Punctuated(value, state.consume().unwrap()));
                continue;
            }
        }

        values.push(Pair::End(value));
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
