use super::{
    parser_util::{InternalAstError, Parser, ParserState},
    *,
};
use crate::tokenizer::{TokenKind, TokenReference, TokenType};

#[derive(Clone, Debug, PartialEq)]
struct ParseSymbol(Symbol);

define_parser!(
    ParseSymbol,
    TokenReference<'a>,
    |this: &ParseSymbol, state: ParserState<'a>| {
        let expecting = TokenType::Symbol { symbol: this.0 };
        let token = state.peek();

        if *token.token_type() == expecting {
            Ok((state.advance().ok_or(InternalAstError::NoMatch)?, token))
        } else {
            Err(InternalAstError::NoMatch)
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseNumber;

define_parser!(
    ParseNumber,
    TokenReference<'a>,
    |_, state: ParserState<'a>| {
        let token = state.peek();
        if token.token_kind() == TokenKind::Number {
            Ok((state.advance().ok_or(InternalAstError::NoMatch)?, token))
        } else {
            Err(InternalAstError::NoMatch)
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseStringLiteral;

define_parser!(
    ParseStringLiteral,
    TokenReference<'a>,
    |_, state: ParserState<'a>| {
        let token = state.peek();
        if token.token_kind() == TokenKind::StringLiteral {
            Ok((state.advance().ok_or(InternalAstError::NoMatch)?, token))
        } else {
            Err(InternalAstError::NoMatch)
        }
    }
);

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParseBlock;
define_parser!(ParseBlock, Block<'a>, |_, mut state: ParserState<'a>| {
    let mut stmts = Vec::new();
    while let Ok((new_state, stmt)) = keep_going!(ParseStmt.parse(state.clone())) {
        state = new_state;
        if let Ok((new_state, _)) = ParseSymbol(Symbol::Semicolon).parse(state.clone()) {
            state = new_state;
        }
        stmts.push(stmt);
    }

    if let Ok((mut state, last_stmt)) = keep_going!(ParseLastStmt.parse(state.clone())) {
        if let Ok((new_state, _)) = ParseSymbol(Symbol::Semicolon).parse(state.clone()) {
            state = new_state;
        }

        Ok((
            state,
            Block {
                stmts,
                last_stmt: Some(last_stmt),
            },
        ))
    } else {
        Ok((
            state,
            Block {
                stmts,
                last_stmt: None,
            },
        ))
    }
});

#[derive(Clone, Debug, PartialEq)]
struct ParseLastStmt;
define_parser!(
    ParseLastStmt,
    LastStmt<'a>,
    |_, state: ParserState<'a>| if let Ok((state, token)) =
        ParseSymbol(Symbol::Return).parse(state.clone())
    {
        let (state, returns) = expect!(
            state,
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false)
                .parse(state.clone()),
            "return values"
        );

        Ok((state, LastStmt::Return { token, returns }))
    } else if let Ok((state, token)) = ParseSymbol(Symbol::Break).parse(state.clone()) {
        Ok((state, LastStmt::Break(token)))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseField;
define_parser!(ParseField, Field<'a>, |_, state: ParserState<'a>| {
    if let Ok((state, start_bracket)) = ParseSymbol(Symbol::LeftBracket).parse(state.clone()) {
        let (state, key) = expect!(state, ParseExpression.parse(state.clone()), "expected key");
        let (state, end_bracket) = expect!(
            state,
            ParseSymbol(Symbol::RightBracket).parse(state.clone()),
            "expected ']'"
        );
        let (state, equal) = expect!(
            state,
            ParseSymbol(Symbol::Equal).parse(state.clone()),
            "expected '='"
        );
        let (state, value) = expect!(
            state,
            ParseExpression.parse(state.clone()),
            "expected value"
        );

        return Ok((
            state.clone(),
            Field::ExpressionKey {
                start_bracket,
                key,
                end_bracket,
                equal,
                value,
            },
        ));
    } else if let Ok((state, key)) = keep_going!(ParseIdentifier.parse(state.clone())) {
        if let Ok((state, equal)) = ParseSymbol(Symbol::Equal).parse(state.clone()) {
            let (state, value) = expect!(
                state,
                ParseExpression.parse(state.clone()),
                "expected value"
            );

            return Ok((state.clone(), Field::NameKey { key, equal, value }));
        }
    }

    if let Ok((state, expr)) = keep_going!(ParseExpression.parse(state.clone())) {
        return Ok((state.clone(), Field::NoKey(expr)));
    }

    Err(InternalAstError::NoMatch)
});

struct ParseTableConstructor;
define_parser!(
    ParseTableConstructor,
    TableConstructor<'a>,
    |_, state: ParserState<'a>| {
        let (mut state, start_brace) = ParseSymbol(Symbol::LeftBrace).parse(state.clone())?;
        let mut fields = Vec::new();

        while let Ok((new_state, field)) = keep_going!(ParseField.parse(state.clone())) {
            let field_sep = if let Ok((new_state, separator)) =
                ParseSymbol(Symbol::Comma).parse(new_state.clone())
            {
                state = new_state;
                Some(separator)
            } else if let Ok((new_state, separator)) =
                ParseSymbol(Symbol::Semicolon).parse(new_state.clone())
            {
                state = new_state;
                Some(separator)
            } else {
                state = new_state;
                None
            };

            let is_none = field_sep.is_none();
            fields.push((field, field_sep));
            if is_none {
                break;
            }
        }

        let (state, end_brace) = expect!(
            state,
            ParseSymbol(Symbol::RightBrace).parse(state.clone()),
            "expected '}'"
        );

        Ok((
            state,
            TableConstructor {
                fields,
                start_brace,
                end_brace,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseExpression;
define_parser!(
    ParseExpression,
    Expression<'a>,
    |_, state: ParserState<'a>| if let Ok((state, value)) =
        keep_going!(ParseValue.parse(state.clone()))
    {
        let (state, binop) = if let Ok((state, bin_op)) = ParseBinOp.parse(state.clone()) {
            let (state, rhs) = expect!(
                state,
                ParseExpression.parse(state.clone()),
                "expected expression"
            );
            (
                state,
                Some(BinOpRhs {
                    bin_op,
                    rhs: Box::new(rhs),
                }),
            )
        } else {
            (state, None)
        };

        let value = Box::new(value);

        Ok((state, Expression::Value { value, binop }))
    } else if let Ok((state, unop)) = keep_going!(ParseUnOp.parse(state.clone())) {
        let (state, expression) = expect!(
            state,
            ParseExpression.parse(state.clone()),
            "expected expression"
        );

        let expression = Box::new(expression);

        Ok((state, Expression::UnaryOperator { unop, expression }))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseParenExpression;
define_parser!(
    ParseParenExpression,
    Expression<'a>,
    |_, state: ParserState<'a>| if let Ok((state, _)) =
        ParseSymbol(Symbol::LeftParen).parse(state.clone())
    {
        let (state, expression) = expect!(
            state,
            ParseExpression.parse(state.clone()),
            "expected expression"
        );
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::RightParen).parse(state.clone()),
            "expected ')'"
        );
        Ok((state, expression))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseValue;
define_parser!(
    ParseValue,
    Value<'a>,
    |_, state: ParserState<'a>| parse_first_of!(state, {
        ParseSymbol(Symbol::Nil) => Value::Symbol,
        ParseSymbol(Symbol::False) => Value::Symbol,
        ParseSymbol(Symbol::True) => Value::Symbol,
        ParseNumber => Value::Number,
        ParseStringLiteral => Value::String,
        ParseSymbol(Symbol::Ellipse) => Value::Symbol,
        ParseFunction => Value::Function,
        ParseTableConstructor => Value::TableConstructor,
        ParseFunctionCall => Value::FunctionCall,
        ParseVar => Value::Var,
        ParseParenExpression => Value::ParseExpression,
    })
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseStmt;
define_parser!(
    ParseStmt,
    Stmt<'a>,
    |_, state: ParserState<'a>| parse_first_of!(state, {
        ParseAssignment => Stmt::Assignment,
        ParseFunctionCall => Stmt::FunctionCall,
        ParseDo => Stmt::Do,
        ParseWhile => Stmt::While,
        ParseRepeat => Stmt::Repeat,
        ParseIf => Stmt::If,
        ParseNumericFor => Stmt::NumericFor,
        ParseGenericFor => Stmt::GenericFor,
        ParseFunctionDeclaration => Stmt::FunctionDeclaration,
        ParseLocalFunction => Stmt::LocalFunction,
        ParseLocalAssignment => Stmt::LocalAssignment,
    })
);

#[derive(Clone, Debug, PartialEq)]
struct ParsePrefix;
define_parser!(
    ParsePrefix,
    Prefix<'a>,
    |_, state: ParserState<'a>| parse_first_of!(state, {
        ParseParenExpression => Prefix::Expression,
        ParseIdentifier => Prefix::Name,
    })
);

struct ParseIndex;
define_parser!(
    ParseIndex,
    Index<'a>,
    |_, state: ParserState<'a>| if let Ok((state, start_bracket)) =
        ParseSymbol(Symbol::LeftBracket).parse(state.clone())
    {
        let (state, expression) = expect!(
            state,
            ParseExpression.parse(state.clone()),
            "expected expression"
        );
        let (state, end_bracket) = expect!(
            state,
            ParseSymbol(Symbol::RightBracket).parse(state.clone()),
            "expected ']'"
        );
        Ok((
            state,
            Index::Brackets {
                start_bracket,
                expression,
                end_bracket,
            },
        ))
    } else if let Ok((state, dot)) = ParseSymbol(Symbol::Dot).parse(state.clone()) {
        let (state, name) = expect!(state, ParseIdentifier.parse(state.clone()), "expected name");
        Ok((state, Index::Dot { dot, name }))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionArgs;
define_parser!(ParseFunctionArgs, FunctionArgs<'a>, |_,
                                                     state: ParserState<
    'a,
>| if let Ok((state, _)) =
    keep_going!(ParseSymbol(Symbol::LeftParen).parse(state.clone()))
{
    let (state, expr_list) = expect!(
        state,
        ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false)
            .parse(state.clone()),
        "expected arguments"
    );
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::RightParen).parse(state.clone()),
        "expected ')'"
    );
    Ok((state, FunctionArgs::Parentheses(expr_list)))
} else if let Ok((state, table_constructor)) =
    keep_going!(ParseTableConstructor.parse(state.clone()))
{
    Ok((state, FunctionArgs::TableConstructor(table_constructor)))
} else if let Ok((state, string)) = keep_going!(ParseStringLiteral.parse(state.clone())) {
    Ok((state, FunctionArgs::String(string)))
} else {
    Err(InternalAstError::NoMatch)
});

#[derive(Clone, Debug, PartialEq)]
struct ParseNumericFor;
define_parser!(
    ParseNumericFor,
    NumericFor<'a>,
    |_, state: ParserState<'a>| {
        let (state, for_token) = ParseSymbol(Symbol::For).parse(state.clone())?;
        let (state, index_variable) = expect!(
            state,
            ParseIdentifier.parse(state.clone()),
            "expected names"
        );
        let (state, _) = ParseSymbol(Symbol::Equal).parse(state.clone())?; // Numeric fors run before generic fors, so we can't guarantee this
        let (state, start) = expect!(
            state,
            ParseExpression.parse(state.clone()),
            "expected start expression"
        );
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::Comma).parse(state.clone()),
            "expected comma"
        );
        let (state, end) = expect!(
            state,
            ParseExpression.parse(state.clone()),
            "expected end expression"
        );
        let (state, step) = if let Ok((state, _)) = ParseSymbol(Symbol::Comma).parse(state.clone())
        {
            let (state, expression) = expect!(
                state,
                ParseExpression.parse(state.clone()),
                "expected limit expression"
            );
            (state, Some(expression))
        } else {
            (state, None)
        };
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::Do).parse(state.clone()),
            "expected 'do'"
        );
        let (state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");
        let (state, end_token) = expect!(
            state,
            ParseSymbol(Symbol::End).parse(state.clone()),
            "expected 'end'"
        );

        Ok((
            state,
            NumericFor {
                for_token,
                index_variable,
                start,
                end,
                step,
                block,
                end_token,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseGenericFor;
define_parser!(
    ParseGenericFor,
    GenericFor<'a>,
    |_, state: ParserState<'a>| {
        let (state, for_token) = ParseSymbol(Symbol::For).parse(state.clone())?;
        let (state, names) = expect!(
            state,
            OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state.clone()),
            "expected names"
        );
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::In).parse(state.clone()),
            "expected 'in'"
        ); // Numeric fors run before here, so there has to be an in
        let (state, expr_list) = expect!(
            state,
            OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state.clone()),
            "expected expression"
        );
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::Do).parse(state.clone()),
            "expected 'do'"
        );
        let (state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");
        let (state, end_token) = expect!(
            state,
            ParseSymbol(Symbol::End).parse(state.clone()),
            "expected 'end'"
        );
        Ok((
            state,
            GenericFor {
                for_token,
                names,
                expr_list,
                block,
                end_token,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseIf;
define_parser!(ParseIf, If<'a>, |_, state: ParserState<'a>| {
    let (state, if_token) = ParseSymbol(Symbol::If).parse(state.clone())?;
    let (state, condition) = expect!(
        state,
        ParseExpression.parse(state.clone()),
        "expected condition"
    );
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::Then).parse(state.clone()),
        "expected 'then'"
    );
    let (mut state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");

    let mut else_ifs = Vec::new();
    while let Ok((new_state, _)) = ParseSymbol(Symbol::ElseIf).parse(state.clone()) {
        let (new_state, condition) = expect!(
            state,
            ParseExpression.parse(new_state),
            "expected condition"
        );
        let (new_state, _) = expect!(
            state,
            ParseSymbol(Symbol::Then).parse(new_state),
            "expected 'then'"
        );
        let (new_state, block) = expect!(state, ParseBlock.parse(new_state), "expected block");
        state = new_state;
        else_ifs.push((condition, block));
    }

    let (state, else_token, r#else) =
        if let Ok((state, else_token)) = ParseSymbol(Symbol::Else).parse(state.clone()) {
            let (state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");
            (state, Some(else_token), Some(block))
        } else {
            (state, None, None)
        };

    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state.clone()),
        "expected 'end'"
    );

    Ok((
        state,
        If {
            if_token,
            condition,
            block,
            else_token,
            r#else,
            else_if: if else_ifs.is_empty() {
                None
            } else {
                Some(else_ifs)
            },
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseWhile;
define_parser!(ParseWhile, While<'a>, |_, state: ParserState<'a>| {
    let (state, while_token) = ParseSymbol(Symbol::While).parse(state.clone())?;
    let (state, condition) = expect!(
        state,
        ParseExpression.parse(state.clone()),
        "expected condition"
    );
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::Do).parse(state.clone()),
        "expected 'do'"
    );
    let (state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state.clone()),
        "expected 'end'"
    );
    Ok((
        state,
        While {
            while_token,
            condition,
            block,
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseRepeat;
define_parser!(ParseRepeat, Repeat<'a>, |_, state: ParserState<'a>| {
    let (state, repeat_token) = ParseSymbol(Symbol::Repeat).parse(state.clone())?;
    let (state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::Until).parse(state.clone()),
        "expected 'until'"
    );
    let (state, until) = expect!(
        state,
        ParseExpression.parse(state.clone()),
        "expected condition"
    );
    Ok((
        state,
        Repeat {
            repeat_token,
            until,
            block,
        },
    ))
});

struct ParseMethodCall;
define_parser!(
    ParseMethodCall,
    MethodCall<'a>,
    |_, state: ParserState<'a>| {
        let (state, _) = ParseSymbol(Symbol::Colon).parse(state.clone())?;
        let (state, name) = expect!(
            state,
            ParseIdentifier.parse(state.clone()),
            "expected method"
        );
        let (state, args) = expect!(
            state,
            ParseFunctionArgs.parse(state.clone()),
            "expected args"
        );
        Ok((state, MethodCall { name, args }))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseCall;
define_parser!(
    ParseCall,
    Call<'a>,
    |_, state: ParserState<'a>| parse_first_of!(state, {
        ParseFunctionArgs => Call::AnonymousCall,
        ParseMethodCall => Call::MethodCall,
    })
);

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionBody;
define_parser!(ParseFunctionBody, FunctionBody<'a>, |_,
                                                     state: ParserState<
    'a,
>| {
    let (mut state, start_paranthese) = expect!(
        state,
        ParseSymbol(Symbol::LeftParen).parse(state.clone()),
        "expected '('"
    );
    let mut parameters = Vec::new();

    if let Ok((new_state, names)) =
        keep_going!(
            OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state.clone())
        )
    {
        state = new_state;
        parameters.extend(names.into_pairs().map(Parameter::Name));

        if let Ok((new_state, _)) = ParseSymbol(Symbol::Comma).parse(state.clone()) {
            if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(new_state) {
                state = new_state;
                parameters.push(Parameter::Ellipse(ellipse));
            }
        }
    } else if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state.clone()) {
        state = new_state;
        parameters.push(Parameter::Ellipse(ellipse));
    }

    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::RightParen).parse(state.clone()),
        "expected ')'"
    );
    let (state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state.clone()),
        "expected 'end'"
    );
    Ok((
        state,
        FunctionBody {
            start_paranthese,
            parameters,
            block,
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunction;
define_parser!(
    ParseFunction,
    FunctionBody<'a>,
    |_, state: ParserState<'a>| {
        let (state, _) = ParseSymbol(Symbol::Function).parse(state.clone())?;
        ParseFunctionBody.parse(state.clone())
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseSuffix;
define_parser!(
    ParseSuffix,
    Suffix<'a>,
    |_, state: ParserState<'a>| parse_first_of!(state, {
        ParseCall => Suffix::Call,
        ParseIndex => Suffix::Index,
    })
);

#[derive(Clone, Debug, PartialEq)]
struct ParseVarExpression;
define_parser!(
    ParseVarExpression,
    VarExpression<'a>,
    |_, state: ParserState<'a>| {
        let (state, prefix) = ParsePrefix.parse(state.clone())?;
        let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state.clone())?;

        if let Some(Suffix::Index(_)) = suffixes.last() {
            Ok((state, VarExpression { prefix, suffixes }))
        } else {
            Err(InternalAstError::NoMatch)
        }
    }
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseVar;
define_parser!(
    ParseVar,
    Var<'a>,
    |_, state: ParserState<'a>| parse_first_of!(state, {
        ParseVarExpression => Var::Expression,
        ParseIdentifier => Var::Name,
    })
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseAssignment;
define_parser!(
    ParseAssignment,
    Assignment<'a>,
    |_, state: ParserState<'a>| {
        let (state, var_list) =
            OneOrMore(ParseVar, ParseSymbol(Symbol::Comma), false).parse(state.clone())?;
        let (state, _) = ParseSymbol(Symbol::Equal).parse(state.clone())?;
        let (state, expr_list) = expect!(
            state,
            OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state.clone()),
            "expected values"
        );

        Ok((
            state,
            Assignment {
                var_list,
                expr_list,
            },
        ))
    }
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalFunction;
define_parser!(
    ParseLocalFunction,
    LocalFunction<'a>,
    |_, state: ParserState<'a>| {
        let (state, local_token) = ParseSymbol(Symbol::Local).parse(state.clone())?;
        let (state, _) = ParseSymbol(Symbol::Function).parse(state.clone())?;
        let (state, name) = expect!(state, ParseIdentifier.parse(state.clone()), "expected name");
        let (state, func_body) = ParseFunctionBody.parse(state.clone())?;
        Ok((
            state,
            LocalFunction {
                local_token,
                name,
                func_body,
            },
        ))
    }
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalAssignment;
define_parser!(
    ParseLocalAssignment,
    LocalAssignment<'a>,
    |_, state: ParserState<'a>| {
        let (state, local_token) = ParseSymbol(Symbol::Local).parse(state.clone())?;
        let (state, name_list) = expect!(
            state,
            OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state.clone()),
            "expected name"
        );
        let (state, expr_list) = match ParseSymbol(Symbol::Equal).parse(state.clone()) {
            Ok((state, _)) => OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false)
                .parse(state.clone())
                .or_else(|_| {
                    Err(InternalAstError::UnexpectedToken {
                        token: state.peek(),
                        additional: Some("expected expression"),
                    })
                })?,
            Err(InternalAstError::NoMatch) => (state, Punctuated::new()),
            Err(other) => return Err(other),
        };

        Ok((
            state,
            LocalAssignment {
                local_token,
                name_list,
                expr_list,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseDo;
define_parser!(ParseDo, Do<'a>, |_, state: ParserState<'a>| {
    let (state, do_token) = ParseSymbol(Symbol::Do).parse(state.clone())?;
    let (state, block) = expect!(state, ParseBlock.parse(state.clone()), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state.clone()),
        "expected 'end'"
    );

    Ok((
        state,
        Do {
            do_token,
            block,
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionCall;
define_parser!(ParseFunctionCall, FunctionCall<'a>, |_,
                                                     state: ParserState<
    'a,
>| {
    let (state, prefix) = ParsePrefix.parse(state.clone())?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state.clone())?;

    if let Some(Suffix::Call(_)) = suffixes.last() {
        Ok((state, FunctionCall { prefix, suffixes }))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionName;
define_parser!(ParseFunctionName, FunctionName<'a>, |_,
                                                     state: ParserState<
    'a,
>| {
    let (state, names) =
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Dot), false).parse(state.clone())?;
    let (state, colon_name) =
        if let Ok((state, _)) = ParseSymbol(Symbol::Colon).parse(state.clone()) {
            let (state, colon_name) = expect!(
                state,
                ParseIdentifier.parse(state.clone()),
                "expected method name"
            );
            (state, Some(colon_name))
        } else {
            (state, None)
        };

    Ok((state, FunctionName { names, colon_name }))
});

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseFunctionDeclaration;
define_parser!(
    ParseFunctionDeclaration,
    FunctionDeclaration<'a>,
    |_, state: ParserState<'a>| {
        let (state, _) = ParseSymbol(Symbol::Function).parse(state.clone())?;
        let (state, name) = expect!(
            state,
            ParseFunctionName.parse(state.clone()),
            "expected function name"
        );
        let (state, body) = expect!(
            state,
            ParseFunctionBody.parse(state.clone()),
            "expected function body"
        );
        Ok((state, FunctionDeclaration { name, body }))
    }
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseIdentifier;
#[rustfmt::skip]
define_parser!(ParseIdentifier, TokenReference<'a>, |_, state: ParserState<'a>| {
    let next_token = state.peek();
    match next_token.token_kind() {
        TokenKind::Identifier => Ok((
            state.advance().ok_or(InternalAstError::NoMatch)?,
            next_token,
        )),
        _ => Err(InternalAstError::NoMatch),
    }
});

macro_rules! make_op_parser {
	($enum:ident, $parser:ident, { $($operator:ident,)+ }) => {
		#[derive(Clone, Debug, PartialEq)]
        struct $parser;
        define_parser!($parser, $enum<'a>, |_, state: ParserState<'a>| {
            $(
                if let Ok((state, _)) = ParseSymbol(Symbol::$operator).parse(state.clone()) {
                    return Ok((state.clone(), $enum::$operator(state.peek())));
                }
            )+

			// This is to ensure the operators ALWAYS match those in the actual operator
			// It won't compile if they don't match up
			if let Some(x) = None {
				match x {
					$(
						$enum::$operator(_) => {},
					)+
				}
			}

            Err(InternalAstError::NoMatch)
        });
	};
}

make_op_parser!(BinOp, ParseBinOp,
    {
        And,
        Caret,
        GreaterThan,
        GreaterThanEqual,
        LessThan,
        LessThanEqual,
        Minus,
        Or,
        Percent,
        Plus,
        Slash,
        Star,
        TildeEqual,
        TwoDots,
        TwoEqual,
    }
);

make_op_parser!(UnOp, ParseUnOp,
    {
        Minus,
        Not,
        Hash,
    }
);

#[cfg(all(test, not(feature = "only-source-tests")))]
mod tests {
    use super::*;
    use crate::tokenizer::tokens;
    use pretty_assertions::assert_eq;

    macro_rules! assert_state_eq {
        ($state: expr, $index: expr, $tokens: ident) => {
            assert_eq!($state.index, $index);
            assert_eq!($state.tokens.len(), $tokens.len());
        };
    }

    macro_rules! tokens {
        ($body: expr) => {
            Arena::from_iter(tokens($body).expect("couldn't tokenize'"))
        };
    }

    #[test]
    fn test_zero_or_more_empty() {
        let tokens = tokens!("local x");
        let state = ParserState::new(Arc::new(tokens.clone()));

        let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma))
            .parse(state.clone())
            .unwrap();

        assert_state_eq!(state, 0, tokens);
        assert_eq!(commas.len(), 0);
    }

    #[test]
    fn test_zero_or_more_exists() {
        let tokens = tokens!(",,, , ,\t ,local x");
        let state = ParserState::new(Arc::new(tokens.clone()));

        let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma))
            .parse(state.clone())
            .unwrap();

        assert_state_eq!(state, 9, tokens);
        assert_eq!(commas.len(), 6);
    }

    #[test]
    fn test_one_or_more_empty() {
        let tokens = tokens!("local x");
        let state = ParserState::new(Arc::new(tokens.clone()));

        assert!(
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
                .parse(state.clone())
                .is_err()
        );
    }

    #[test]
    fn test_one_or_more_exists_no_delimiter() {
        let tokens = tokens!("end,end, end,\t\tend local");
        let state = ParserState::new(Arc::new(tokens.clone()));

        let (state, commas) =
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
                .parse(state.clone())
                .expect("OneOrMore failed");

        assert_state_eq!(state, 10, tokens);
        assert_eq!(commas.len(), 4);
    }

    #[test]
    fn test_one_or_more_exists_with_delimiter() {
        let tokens = tokens!("end,end, end,\t\tend, local");
        let state = ParserState::new(Arc::new(tokens.clone()));

        let (state, commas) = OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
            .parse(state.clone())
            .unwrap();

        assert_state_eq!(state, 11, tokens);
        assert_eq!(commas.len(), 4);
    }

    #[test]
    fn test_one_or_more_exists_with_nothing() {
        let tokens = tokens!("local");
        let state = ParserState::new(Arc::new(tokens.clone()));

        assert!(
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
                .parse(state.clone())
                .is_err()
        );
    }
}
