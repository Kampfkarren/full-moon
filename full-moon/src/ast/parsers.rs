use super::{
    parser_util::{InternalAstError, Parser, ParserState},
    span::ContainedSpan,
    *,
};

#[cfg(feature = "roblox")]
use super::types::*;

use crate::tokenizer::{TokenKind, TokenReference, TokenType};

use crate::tokenizer::*;
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::sequence::pair;
use nom::IResult;

/*
impl<'a> InputLength for Token<'a> {
    fn input_len(&self) -> usize {
        0
    }
}
impl<'a> InputLength for TokenType<'a> {
    fn input_len(&self) -> usize {
        0
    }
}
impl<'a> Compare<TokenType<'a>> for ParserState<'a> {
    fn compare(&self, t: TokenType<'a>) -> CompareResult {
        let token = self.peek();

        if *token.token_type() == t {
            CompareResult::Ok
        } else {
            CompareResult::Error
        }
    }
    fn compare_no_case(&self, t: TokenType<'a>) -> CompareResult {
        self.compare(t)
    }
}
*/

impl<'a, 'b> nom::error::ParseError<ParserState<'a, 'b>> for InternalAstError<'a> {
    fn from_error_kind(_: ParserState<'a, 'b>, _: nom::error::ErrorKind) -> Self {
        InternalAstError::NoMatch
    }
    fn append(_: ParserState<'a, 'b>, _: nom::error::ErrorKind, _: Self) -> Self {
        InternalAstError::NoMatch
    }
}

impl<'a> From<nom::Err<InternalAstError<'a>>> for InternalAstError<'a> {
    fn from(e: nom::Err<InternalAstError<'a>>) -> Self {
        match e {
            nom::Err::Incomplete(_) => unreachable!(),
            nom::Err::Error(e) => e,
            nom::Err::Failure(e) => e,
        }
    }
}

macro_rules! from_nom {
    (|$self:ident| $body:expr) => {
        |$self: &Self, state| {
            Ok({
                let (state, result) = $body(state)?;
                (state, result)
            })
        }
    };
    ($body:expr) => {
        from_nom!(|_x| $body)
    };
}

fn from_parser<'a, 'b, R>(
    parser: impl Parser<'a, Item = R>,
) -> impl FnMut(ParserState<'a, 'b>) -> IResult<ParserState<'a, 'b>, R, InternalAstError<'a>>
where
    'a: 'b,
{
    move |input: ParserState<'a, 'b>| parser.parse(input).map_err(nom::Err::Error)
}

fn symbol<'a, 'b>(
    symbol: Symbol,
) -> impl FnMut(
    ParserState<'a, 'b>,
) -> IResult<ParserState<'a, 'b>, TokenReference<'a>, InternalAstError<'a>>
where
    'a: 'b,
{
    move |input: ParserState<'a, 'b>| {
        let expecting = TokenType::Symbol { symbol };
        input
            .take_if(|token| token.token_type() == &expecting)
            .map_err(nom::Err::Error)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct ParseSymbol(Symbol);

define_parser!(
    ParseSymbol,
    Cow<'a, TokenReference<'a>>,
    from_nom!(|this| map(symbol(this.0), Cow::Owned))
);

fn token_kind<'a, 'b>(
    token_kind: TokenKind,
) -> impl FnMut(
    ParserState<'a, 'b>,
) -> IResult<ParserState<'a, 'b>, TokenReference<'a>, InternalAstError<'a>>
where
    'a: 'b,
{
    move |input: ParserState<'a, 'b>| {
        input
            .take_if(|token| token.token_kind() == token_kind)
            .map_err(nom::Err::Error)
    }
}

fn number<'a, 'b>(
    input: ParserState<'a, 'b>,
) -> IResult<ParserState<'a, 'b>, TokenReference<'a>, InternalAstError<'a>>
where
    'a: 'b,
{
    token_kind(TokenKind::Number)(input)
}

#[derive(Clone, Debug, PartialEq)]
struct ParseNumber;

define_parser!(
    ParseNumber,
    Cow<'a, TokenReference<'a>>,
    from_nom!(map(number, Cow::Owned))
);

fn string_literal<'a, 'b>(
    input: ParserState<'a, 'b>,
) -> IResult<ParserState<'a, 'b>, TokenReference<'a>, InternalAstError<'a>>
where
    'a: 'b,
{
    token_kind(TokenKind::StringLiteral)(input)
}

#[derive(Clone, Debug, PartialEq)]
struct ParseStringLiteral;

define_parser!(
    ParseStringLiteral,
    Cow<'a, TokenReference<'a>>,
    from_nom!(map(string_literal, Cow::Owned))
);

pub(crate) fn block<'a, 'b>(
    input: ParserState<'a, 'b>,
) -> IResult<ParserState<'a, 'b>, Block<'a>, InternalAstError<'a>>
where
    'a: 'b,
{
    let stmt = from_parser(ParseStmt);
    let last_stmt = from_parser(ParseLastStmt);

    use nom::Parser;
    let semi = |I| opt(symbol(Symbol::Semicolon))(I);
    let mut comb =
        pair(many0(pair(stmt, semi)), opt(pair(last_stmt, semi))).map(|(stmts, last_stmt)| {
            let stmts = stmts
                .into_iter()
                .map(|(stmt, semi)| (stmt, semi.map(Cow::Owned)))
                .collect();
            let last_stmt = last_stmt.map(|(stmt, semi)| (stmt, semi.map(Cow::Owned)));
            Block { stmts, last_stmt }
        });

    comb.parse(input)
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParseBlock;
define_parser!(ParseBlock, Block<'a>, from_nom!(block));

#[derive(Clone, Debug, PartialEq)]
struct ParseLastStmt;
define_parser!(
    ParseLastStmt,
    LastStmt<'a>,
    |_, state: ParserState<'a, 'b>| if let Ok((state, token)) =
        ParseSymbol(Symbol::Return).parse(state)
    {
        let (state, returns) = expect!(
            state,
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
            "return values"
        );

        Ok((state, LastStmt::Return(Return { token, returns })))
    } else if let Ok((state, token)) = ParseSymbol(Symbol::Break).parse(state) {
        Ok((state, LastStmt::Break(token)))
    } else {
        cfg_if::cfg_if! {
            if #[cfg(feature = "roblox")] {
                let (state, continue_token) = ParseIdentifier.parse(state)?;
                if continue_token.token().to_string() == "continue" {
                    Ok((state, LastStmt::Continue(continue_token)))
                } else {
                    Err(InternalAstError::NoMatch)
                }
            } else {
                Err(InternalAstError::NoMatch)
            }
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseField;
define_parser!(ParseField, Field<'a>, |_, state: ParserState<'a, 'b>| {
    if let Ok((state, start_bracket)) = ParseSymbol(Symbol::LeftBracket).parse(state) {
        let (state, key) = expect!(state, ParseExpression.parse(state), "expected key");
        let (state, end_bracket) = expect!(
            state,
            ParseSymbol(Symbol::RightBracket).parse(state),
            "expected ']'"
        );
        let (state, equal) = expect!(
            state,
            ParseSymbol(Symbol::Equal).parse(state),
            "expected '='"
        );
        let (state, value) = expect!(state, ParseExpression.parse(state), "expected value");

        return Ok((
            state,
            Field::ExpressionKey {
                brackets: ContainedSpan::new(start_bracket, end_bracket),
                key,
                equal,
                value,
            },
        ));
    } else if let Ok((state, key)) = keep_going!(ParseIdentifier.parse(state)) {
        if let Ok((state, equal)) = ParseSymbol(Symbol::Equal).parse(state) {
            let (state, value) = expect!(state, ParseExpression.parse(state), "expected value");

            return Ok((state, Field::NameKey { key, equal, value }));
        }
    }

    if let Ok((state, expr)) = keep_going!(ParseExpression.parse(state)) {
        return Ok((state, Field::NoKey(expr)));
    }

    Err(InternalAstError::NoMatch)
});

struct ParseTableConstructor;
define_parser!(
    ParseTableConstructor,
    TableConstructor<'a>,
    |_, state: ParserState<'a, 'b>| {
        let (mut state, start_brace) = ParseSymbol(Symbol::LeftBrace).parse(state)?;
        let mut fields = Punctuated::new();

        while let Ok((new_state, field)) = keep_going!(ParseField.parse(state)) {
            let field_sep =
                if let Ok((new_state, separator)) = ParseSymbol(Symbol::Comma).parse(new_state) {
                    state = new_state;
                    Some(separator)
                } else if let Ok((new_state, separator)) =
                    ParseSymbol(Symbol::Semicolon).parse(new_state)
                {
                    state = new_state;
                    Some(separator)
                } else {
                    state = new_state;
                    None
                };

            let is_end = field_sep.is_none();
            fields.push(Pair::new(field, field_sep));
            if is_end {
                break;
            }
        }

        let (state, end_brace) = expect!(
            state,
            ParseSymbol(Symbol::RightBrace).parse(state),
            "expected '}'"
        );

        Ok((
            state,
            TableConstructor {
                braces: ContainedSpan::new(start_brace, end_brace),
                fields,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseExpression;
define_parser!(ParseExpression, Expression<'a>, |_,
                                                 state: ParserState<
    'a,
    'b,
>| if let Ok((state, value)) =
    keep_going!(ParseValue.parse(state))
{
    let (state, as_assertion) =
        if let Ok((state, as_assertion)) = keep_going!(ParseAsAssertion.parse(state)) {
            (state, Some(as_assertion))
        } else {
            (state, None)
        };

    let (state, binop) = if as_assertion.is_none() {
        if let Ok((state, bin_op)) = ParseBinOp.parse(state) {
            let (state, rhs) = expect!(state, ParseExpression.parse(state), "expected expression");

            (
                state,
                Some(BinOpRhs {
                    bin_op,
                    rhs: Box::new(rhs),
                }),
            )
        } else {
            (state, None)
        }
    } else {
        (state, None)
    };

    let value = Box::new(value);

    Ok((
        state,
        Expression::Value {
            value,
            binop,
            #[cfg(feature = "roblox")]
            as_assertion,
        },
    ))
} else if let Ok((state, unop)) = keep_going!(ParseUnOp.parse(state)) {
    let (state, expression) = expect!(state, ParseExpression.parse(state), "expected expression");

    let expression = Box::new(expression);

    Ok((state, Expression::UnaryOperator { unop, expression }))
} else {
    Err(InternalAstError::NoMatch)
});

#[derive(Clone, Debug, PartialEq)]
struct ParseAsAssertion;

#[rustfmt::skip]
define_roblox_parser!(
    ParseAsAssertion,
    AsAssertion<'a>,
    Cow<'a, TokenReference<'a>>,
    |_, state: ParserState<'a, 'b>| {
        let (state, as_token) = ParseIdentifier.parse(state)?;
        if as_token.token().to_string() == "as" {
            let (state, cast_to) = expect!(
                state,
                ParseTypeInfo.parse(state),
                "expected type in `as` expression"
            );

            Ok((
                state,
                AsAssertion {
                    as_token,
                    cast_to,
                },
            ))
        } else {
            Err(InternalAstError::NoMatch)
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseParenExpression;
define_parser!(
    ParseParenExpression,
    Expression<'a>,
    |_, state: ParserState<'a, 'b>| if let Ok((state, left_paren)) =
        ParseSymbol(Symbol::LeftParen).parse(state)
    {
        let (state, expression) =
            expect!(state, ParseExpression.parse(state), "expected expression");

        let (state, right_paren) = expect!(
            state,
            ParseSymbol(Symbol::RightParen).parse(state),
            "expected ')'"
        );

        Ok((
            state,
            Expression::Parentheses {
                contained: ContainedSpan::new(left_paren, right_paren),
                expression: Box::new(expression),
            },
        ))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseValue;
define_parser!(
    ParseValue,
    Value<'a>,
    |_, state: ParserState<'a, 'b>| parse_first_of!(state, {
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
    |_, state: ParserState<'a, 'b>| parse_first_of!(state, {
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
        @#[cfg(feature = "roblox")]
        ParseCompoundAssignment => Stmt::CompoundAssignment,
        @#[cfg(feature = "roblox")]
        ParseExportedTypeDeclaration => Stmt::ExportedTypeDeclaration,
        @#[cfg(feature = "roblox")]
        ParseTypeDeclaration => Stmt::TypeDeclaration,
    })
);

#[derive(Clone, Debug, PartialEq)]
struct ParsePrefix;
define_parser!(
    ParsePrefix,
    Prefix<'a>,
    |_, state: ParserState<'a, 'b>| parse_first_of!(state, {
        ParseParenExpression => Prefix::Expression,
        ParseIdentifier => Prefix::Name,
    })
);

struct ParseIndex;
define_parser!(
    ParseIndex,
    Index<'a>,
    |_, state: ParserState<'a, 'b>| if let Ok((state, start_bracket)) =
        ParseSymbol(Symbol::LeftBracket).parse(state)
    {
        let (state, expression) =
            expect!(state, ParseExpression.parse(state), "expected expression");
        let (state, end_bracket) = expect!(
            state,
            ParseSymbol(Symbol::RightBracket).parse(state),
            "expected ']'"
        );
        Ok((
            state,
            Index::Brackets {
                brackets: ContainedSpan::new(start_bracket, end_bracket),
                expression,
            },
        ))
    } else if let Ok((state, dot)) = ParseSymbol(Symbol::Dot).parse(state) {
        let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected name");
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
    'b,
>| if let Ok((state, left_paren)) =
    keep_going!(ParseSymbol(Symbol::LeftParen).parse(state))
{
    let (state, arguments) = expect!(
        state,
        ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected arguments"
    );
    let (state, right_paren) = expect!(
        state,
        ParseSymbol(Symbol::RightParen).parse(state),
        "expected ')'"
    );
    Ok((
        state,
        FunctionArgs::Parentheses {
            arguments,
            parentheses: ContainedSpan::new(left_paren, right_paren),
        },
    ))
} else if let Ok((state, table_constructor)) = keep_going!(ParseTableConstructor.parse(state)) {
    Ok((state, FunctionArgs::TableConstructor(table_constructor)))
} else if let Ok((state, string)) = keep_going!(ParseStringLiteral.parse(state)) {
    Ok((state, FunctionArgs::String(string)))
} else {
    Err(InternalAstError::NoMatch)
});

#[derive(Clone, Debug, PartialEq)]
struct ParseNumericFor;
define_parser!(ParseNumericFor, NumericFor<'a>, |_,
                                                 state: ParserState<
    'a,
    'b,
>| {
    let (mut state, for_token) = ParseSymbol(Symbol::For).parse(state)?;

    let index_variable;

    #[cfg(feature = "roblox")]
    let type_specifier;

    cfg_if::cfg_if! {
        if #[cfg(feature = "roblox")] {
            let (new_state, (new_index_variable, new_type_specifier)) =
                expect!(state, ParseNameWithType.parse(state), "expected names");

            state = new_state;
            index_variable = new_index_variable;
            type_specifier = new_type_specifier;
        } else {
            let (new_state, new_index_variable) =
                expect!(state, ParseIdentifier.parse(state), "expected names");

            state = new_state;
            index_variable = new_index_variable;
        }
    }

    let (state, equal_token) = ParseSymbol(Symbol::Equal).parse(state)?; // Numeric fors run before generic fors, so we can't guarantee this
    let (state, start) = expect!(
        state,
        ParseExpression.parse(state),
        "expected start expression"
    );
    let (state, start_end_comma) = expect!(
        state,
        ParseSymbol(Symbol::Comma).parse(state),
        "expected comma"
    );
    let (state, end) = expect!(
        state,
        ParseExpression.parse(state),
        "expected end expression"
    );
    let (state, step, end_step_comma) =
        if let Ok((state, comma)) = ParseSymbol(Symbol::Comma).parse(state) {
            let (state, expression) = expect!(
                state,
                ParseExpression.parse(state),
                "expected limit expression"
            );
            (state, Some(expression), Some(comma))
        } else {
            (state, None, None)
        };
    let (state, do_token) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((
        state,
        NumericFor {
            for_token,
            index_variable,
            equal_token,
            start,
            start_end_comma,
            end,
            end_step_comma,
            step,
            do_token,
            block,
            end_token,
            #[cfg(feature = "roblox")]
            type_specifier,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseGenericFor;
define_parser!(ParseGenericFor, GenericFor<'a>, |_,
                                                 state: ParserState<
    'a,
    'b,
>| {
    let (mut state, for_token) = ParseSymbol(Symbol::For).parse(state)?;

    let mut names;
    let mut type_specifiers = Vec::new();

    if cfg!(feature = "roblox") {
        names = Punctuated::new();

        let (new_state, full_name_list) = expect!(
            state,
            OneOrMore(ParseNameWithType, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected names"
        );

        for mut pair in full_name_list.into_pairs() {
            type_specifiers.push(pair.value_mut().1.take());
            names.push(pair.map(|(name, _)| name));
        }

        state = new_state;
    } else {
        let (new_state, new_names) = expect!(
            state,
            OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected names"
        );

        state = new_state;
        names = new_names;
    }

    let (state, in_token) = expect!(state, ParseSymbol(Symbol::In).parse(state), "expected 'in'"); // Numeric fors run before here, so there has to be an in
    let (state, expr_list) = expect!(
        state,
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected expression"
    );
    let (state, do_token) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((
        state,
        GenericFor {
            for_token,
            names,
            in_token,
            expr_list,
            do_token,
            block,
            end_token,
            #[cfg(feature = "roblox")]
            type_specifiers,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseIf;
define_parser!(ParseIf, If<'a>, |_, state: ParserState<'a, 'b>| {
    let (state, if_token) = ParseSymbol(Symbol::If).parse(state)?;
    let (state, condition) = expect!(state, ParseExpression.parse(state), "expected condition");
    let (state, then_token) = expect!(
        state,
        ParseSymbol(Symbol::Then).parse(state),
        "expected 'then'"
    );
    let (mut state, block) = expect!(state, ParseBlock.parse(state), "expected block");

    let mut else_ifs = Vec::new();
    while let Ok((new_state, else_if_token)) = ParseSymbol(Symbol::ElseIf).parse(state) {
        let (new_state, condition) = expect!(
            state,
            ParseExpression.parse(new_state),
            "expected condition"
        );
        let (new_state, then_token) = expect!(
            state,
            ParseSymbol(Symbol::Then).parse(new_state),
            "expected 'then'"
        );
        let (new_state, block) = expect!(state, ParseBlock.parse(new_state), "expected block");
        state = new_state;
        else_ifs.push(ElseIf {
            else_if_token,
            condition,
            then_token,
            block,
        });
    }

    let (state, else_token, r#else) =
        if let Ok((state, else_token)) = ParseSymbol(Symbol::Else).parse(state) {
            let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
            (state, Some(else_token), Some(block))
        } else {
            (state, None, None)
        };

    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((
        state,
        If {
            if_token,
            condition,
            then_token,
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
define_parser!(ParseWhile, While<'a>, |_, state: ParserState<'a, 'b>| {
    let (state, while_token) = ParseSymbol(Symbol::While).parse(state)?;
    let (state, condition) = expect!(state, ParseExpression.parse(state), "expected condition");
    let (state, do_token) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((
        state,
        While {
            while_token,
            condition,
            do_token,
            block,
            end_token,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseRepeat;
define_parser!(ParseRepeat, Repeat<'a>, |_, state: ParserState<'a, 'b>| {
    let (state, repeat_token) = ParseSymbol(Symbol::Repeat).parse(state)?;
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, until_token) = expect!(
        state,
        ParseSymbol(Symbol::Until).parse(state),
        "expected 'until'"
    );
    let (state, until) = expect!(state, ParseExpression.parse(state), "expected condition");
    Ok((
        state,
        Repeat {
            repeat_token,
            until_token,
            until,
            block,
        },
    ))
});

struct ParseMethodCall;
define_parser!(ParseMethodCall, MethodCall<'a>, |_,
                                                 state: ParserState<
    'a,
    'b,
>| {
    let (state, colon_token) = ParseSymbol(Symbol::Colon).parse(state)?;
    let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected method");
    let (state, args) = expect!(state, ParseFunctionArgs.parse(state), "expected args");
    Ok((
        state,
        MethodCall {
            colon_token,
            name,
            args,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseCall;
define_parser!(
    ParseCall,
    Call<'a>,
    |_, state: ParserState<'a, 'b>| parse_first_of!(state, {
        ParseFunctionArgs => Call::AnonymousCall,
        ParseMethodCall => Call::MethodCall,
    })
);

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionBody;
#[rustfmt::skip]
define_parser!(ParseFunctionBody, FunctionBody<'a>, |_, state: ParserState<'a, 'b>| {
    let (mut state, start_parenthese) = expect!(
        state,
        ParseSymbol(Symbol::LeftParen).parse(state),
        "expected '('"
    );

    let mut parameters = Punctuated::new();
    let mut name_list = None;
    let mut type_specifiers = Vec::new();

    if cfg!(feature = "roblox") {
        if let Ok((new_state, full_name_list)) = keep_going!(
            OneOrMore(ParseNameWithType, ParseSymbol(Symbol::Comma), false).parse(state)
        ) {
            let mut new_name_list = Punctuated::new();

            for mut pair in full_name_list.into_pairs() {
                type_specifiers.push(pair.value_mut().1.take());
                new_name_list.push(pair.map(|(name, _)| name));
            }

            state = new_state;
            name_list = Some(new_name_list);
        }
    } else if let Ok((new_state, new_name_list)) = keep_going!(
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state)
    ) {
        state = new_state;
        name_list = Some(new_name_list);
    }

    if let Some(names) = name_list {
        parameters.extend(names.into_pairs().map(|pair| {
            let tuple = pair.into_tuple();
            Pair::new(Parameter::Name(tuple.0), tuple.1)
        }));

        if let Ok((new_state, comma)) = ParseSymbol(Symbol::Comma).parse(state) {
            if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(new_state) {
                state = new_state;

                let mut last_parameter = parameters.pop().expect("comma parsed and accepted, but no arguments before it?");
                last_parameter = Pair::new(last_parameter.into_value(), Some(comma));
                parameters.push(last_parameter);

                parameters.push(Pair::new(Parameter::Ellipse(ellipse), None));
            }
        }
    } else if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state) {
        state = new_state;
        parameters.push(Pair::new(Parameter::Ellipse(ellipse), None));
    }

    let (state, end_parenthese) = expect!(
        state,
        ParseSymbol(Symbol::RightParen).parse(state),
        "expected ')'"
    );

    #[cfg_attr(not(feature = "roblox"), allow(unused_variables))]
    let (state, return_type) = if let Ok((state, return_type)) = ParseFunctionReturnType.parse(state) {
        (state, Some(return_type))
    } else {
        (state, None)
    };

    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((
        state,
        FunctionBody {
            parameters_parentheses: ContainedSpan::new(start_parenthese, end_parenthese),
            parameters,
            block,
            end_token,
            #[cfg(feature = "roblox")]
            type_specifiers,
            #[cfg(feature = "roblox")]
            return_type,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionReturnType;
define_roblox_parser!(
    ParseFunctionReturnType,
    TypeSpecifier<'a>,
    Cow<'a, TokenReference<'a>>,
    |_, state: ParserState<'a, 'b>| {
        let (state, colon) = ParseSymbol(Symbol::Colon).parse(state)?;
        let (state, return_type) =
            expect!(state, ParseTypeInfo.parse(state), "expected return type");

        Ok((
            state,
            TypeSpecifier {
                punctuation: colon,
                type_info: return_type,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseFunction;
define_parser!(
    ParseFunction,
    (Cow<'a, TokenReference<'a>>, FunctionBody<'a>),
    |_, state: ParserState<'a, 'b>| {
        let (state, token) = ParseSymbol(Symbol::Function).parse(state)?;
        let (state, body) = expect!(
            state,
            ParseFunctionBody.parse(state),
            "expected function body"
        );
        Ok((state, (token, body)))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseSuffix;
define_parser!(
    ParseSuffix,
    Suffix<'a>,
    |_, state: ParserState<'a, 'b>| parse_first_of!(state, {
        ParseCall => Suffix::Call,
        ParseIndex => Suffix::Index,
    })
);

#[derive(Clone, Debug, PartialEq)]
struct ParseVarExpression;
define_parser!(
    ParseVarExpression,
    VarExpression<'a>,
    |_, state: ParserState<'a, 'b>| {
        let (state, prefix) = ParsePrefix.parse(state)?;
        let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;

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
    |_, state: ParserState<'a, 'b>| parse_first_of!(state, {
        ParseVarExpression => Var::Expression,
        ParseIdentifier => Var::Name,
    })
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseAssignment;
define_parser!(ParseAssignment, Assignment<'a>, |_,
                                                 state: ParserState<
    'a,
    'b,
>| {
    let (state, var_list) = OneOrMore(ParseVar, ParseSymbol(Symbol::Comma), false).parse(state)?;
    let (state, equal_token) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, expr_list) = expect!(
        state,
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected values"
    );

    Ok((
        state,
        Assignment {
            var_list,
            equal_token,
            expr_list,
        },
    ))
});

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalFunction;
define_parser!(
    ParseLocalFunction,
    LocalFunction<'a>,
    |_, state: ParserState<'a, 'b>| {
        let (state, local_token) = ParseSymbol(Symbol::Local).parse(state)?;
        let (state, function_token) = ParseSymbol(Symbol::Function).parse(state)?;
        let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected name");
        let (state, func_body) = ParseFunctionBody.parse(state)?;
        Ok((
            state,
            LocalFunction {
                local_token,
                function_token,
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
    |_, state: ParserState<'a, 'b>| {
        let (mut state, local_token) = ParseSymbol(Symbol::Local).parse(state)?;

        let mut name_list;
        let mut type_specifiers = Vec::new();

        if cfg!(feature = "roblox") {
            name_list = Punctuated::new();

            let (new_state, full_name_list) = expect!(
                state,
                OneOrMore(ParseNameWithType, ParseSymbol(Symbol::Comma), false).parse(state),
                "expected name"
            );

            for mut pair in full_name_list.into_pairs() {
                type_specifiers.push(pair.value_mut().1.take());
                name_list.push(pair.map(|(name, _)| name));
            }

            state = new_state;
        } else {
            let (new_state, new_name_list) = expect!(
                state,
                OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state),
                "expected name"
            );

            state = new_state;
            name_list = new_name_list;
        }

        let ((state, expr_list), equal_token) = match ParseSymbol(Symbol::Equal).parse(state) {
            Ok((state, equal_token)) => (
                OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false)
                    .parse(state)
                    .map_err(|_| InternalAstError::UnexpectedToken {
                        token: (*state.peek()).to_owned(),
                        additional: Some("expected expression"),
                    })?,
                Some(equal_token),
            ),
            Err(InternalAstError::NoMatch) => ((state, Punctuated::new()), None),
            Err(other) => return Err(other),
        };

        Ok((
            state,
            LocalAssignment {
                local_token,
                name_list,
                equal_token,
                expr_list,
                #[cfg(feature = "roblox")]
                type_specifiers,
            },
        ))
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseDo;
define_parser!(ParseDo, Do<'a>, |_, state: ParserState<'a, 'b>| {
    let (state, do_token) = ParseSymbol(Symbol::Do).parse(state)?;
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, end_token) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
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
    'b,
>| {
    let (state, prefix) = ParsePrefix.parse(state)?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;

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
    'b,
>| {
    let (state, names) =
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Dot), false).parse(state)?;
    let (state, colon_name) = if let Ok((state, colon)) = ParseSymbol(Symbol::Colon).parse(state) {
        let (state, colon_name) =
            expect!(state, ParseIdentifier.parse(state), "expected method name");
        (state, Some((colon, colon_name)))
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
    |_, state: ParserState<'a, 'b>| {
        let (state, function_token) = ParseSymbol(Symbol::Function).parse(state)?;
        let (state, name) = expect!(
            state,
            ParseFunctionName.parse(state),
            "expected function name"
        );
        let (state, body) = expect!(
            state,
            ParseFunctionBody.parse(state),
            "expected function body"
        );
        Ok((
            state,
            FunctionDeclaration {
                function_token,
                name,
                body,
            },
        ))
    }
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseIdentifier;
#[rustfmt::skip]
define_parser!(ParseIdentifier, Cow<'a, TokenReference<'a>>, |_, state: ParserState<'a, 'b>| {
    let next_token = state.peek();
    match next_token.token_kind() {
        TokenKind::Identifier => Ok((
            state.advance().ok_or(InternalAstError::NoMatch)?,
            next_token,
        )),
        _ => Err(InternalAstError::NoMatch),
    }
});

// Roblox Types
#[derive(Clone, Debug, PartialEq)]
struct ParseNameWithType;
define_roblox_parser!(
    ParseNameWithType,
    (Cow<'a, TokenReference<'a>>, Option<TypeSpecifier<'a>>),
    (Cow<'a, TokenReference<'a>>, Option<TokenReference<'a>>),
    |_, state: ParserState<'a, 'b>| {
        let (state, name) = ParseIdentifier.parse(state)?;
        let (state, type_specifier) =
            if let Ok((state, type_specifier)) = keep_going!(ParseTypeSpecifier.parse(state)) {
                (state, Some(type_specifier))
            } else {
                (state, None)
            };

        Ok((state, (name, type_specifier)))
    }
);

cfg_if::cfg_if! {
    if #[cfg(feature = "roblox")] {
        // Roblox Compound Assignment
        #[derive(Clone, Debug, Default, PartialEq)]
        struct ParseCompoundAssignment;
        define_parser!(
            ParseCompoundAssignment,
            CompoundAssignment<'a>,
            |_, state: ParserState<'a, 'b>| {
                let (state, lhs) = ParseVar.parse(state)?;
                let (state, compound_operator) = ParseCompoundOp.parse(state)?;
                let (state, rhs) = expect!(
                    state,
                    ParseExpression.parse(state),
                    "expected value"
                );

                Ok((
                    state,
                    CompoundAssignment {
                        lhs,
                        compound_operator,
                        rhs,
                    },
                ))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeDeclaration;
        define_parser!(
            ParseTypeDeclaration,
            TypeDeclaration<'a>,
            |_, state: ParserState<'a, 'b>| {
                let (state, type_token) = ParseIdentifier.parse(state)?;
                if type_token.token().to_string() != "type" {
                    return Err(InternalAstError::NoMatch);
                }

                let (state, base) = ParseIdentifier.parse(state)?;

                let (state, generics) = if let Ok((state, start_arrow)) =
                    ParseSymbol(Symbol::LessThan).parse(state)
                {
                    let (state, generics) = expect!(
                        state,
                        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state),
                        "expected type parameters"
                    );

                    let (state, end_arrow) = expect!(
                        state,
                        ParseSymbol(Symbol::GreaterThan).parse(state),
                        "expected `>` to match `<`"
                    );

                    (
                        state,
                        Some(GenericDeclaration {
                            arrows: ContainedSpan::new(start_arrow, end_arrow),
                            generics,
                        }),
                    )
                } else {
                    (state, None)
                };

                let (state, equal_token) = expect!(
                    state,
                    ParseSymbol(Symbol::Equal).parse(state),
                    "expected `=` while parsing type alias"
                );

                let (state, declare_as) =
                    expect!(state, ParseTypeInfo.parse(state), "expected type");

                Ok((
                    state,
                    TypeDeclaration {
                        type_token,
                        base,
                        generics,
                        equal_token,
                        declare_as,
                    },
                ))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseExportedTypeDeclaration;
        define_parser!(
            ParseExportedTypeDeclaration,
            ExportedTypeDeclaration<'a>,
            |_, state: ParserState<'a, 'b>| {
                let (state, export_token) = ParseIdentifier.parse(state)?;
                if export_token.token().to_string() != "export" {
                    return Err(InternalAstError::NoMatch);
                }

                let (state, type_declaration) =
                    expect!(state, ParseTypeDeclaration.parse(state), "expected type declaration");

                Ok((
                    state,
                    ExportedTypeDeclaration {
                        export_token,
                        type_declaration
                    },
                ))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseIndexedTypeInfo;
        define_parser!(ParseIndexedTypeInfo, IndexedTypeInfo<'a>, |_, state: ParserState<'a, 'b>| {
            let (state, base_type) = if let Ok((state, identifier)) = {
                ParseIdentifier.parse(state)
            } {
                if let Ok((state, start_arrow)) = ParseSymbol(Symbol::LessThan).parse(state)
                {
                    let (state, generics) = expect!(
                        state,
                        OneOrMore(ParseTypeInfo, ParseSymbol(Symbol::Comma), false).parse(state),
                        "expected type parameters"
                    );

                    let (state, end_arrow) = expect!(
                        state,
                        ParseSymbol(Symbol::GreaterThan).parse(state),
                        "expected `>` to close `<`"
                    );

                    (
                        state,
                        IndexedTypeInfo::Generic {
                            base: identifier,
                            arrows: ContainedSpan::new(start_arrow, end_arrow),
                            generics,
                        },
                    )
                } else {
                    (state, IndexedTypeInfo::Basic(identifier))
                }
            } else {
                return Err(InternalAstError::NoMatch);
            };

            Ok((state, base_type))
        });

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeInfo;
        define_parser!(ParseTypeInfo, TypeInfo<'a>, |_, state: ParserState<'a, 'b>| {
            let (mut state, mut base_type) = if let Ok((state, identifier)) = {
                ParseIdentifier
                    .parse(state)
                    .or_else(|_| ParseSymbol(Symbol::Nil).parse(state))
            } {
                if identifier.token().to_string() == "typeof" {
                    let (state, start_parenthese) = expect!(
                        state,
                        ParseSymbol(Symbol::LeftParen).parse(state),
                        "expected '(' when parsing typeof type"
                    );

                    let (state, expression) = expect!(
                        state,
                        ParseExpression.parse(state),
                        "expected expression when parsing typeof type"
                    );

                    let (state, end_parenthese) = expect!(
                        state,
                        ParseSymbol(Symbol::RightParen).parse(state),
                        "expected ')' when parsing typeof type"
                    );

                    (
                        state,
                        TypeInfo::Typeof {
                            typeof_token: identifier,
                            parentheses: ContainedSpan::new(start_parenthese, end_parenthese),
                            inner: Box::new(expression),
                        },
                    )
                } else if let Ok((state, punctuation)) = ParseSymbol(Symbol::Dot).parse(state)
                {
                    let (state, type_info) = expect!(
                        state,
                        ParseIndexedTypeInfo.parse(state),
                        "expected type when parsing type index"
                    );

                    (
                        state,
                        TypeInfo::Module {
                            module: identifier,
                            punctuation,
                            type_info: Box::new(type_info),
                        },
                    )
                } else if let Ok((state, start_arrow)) = ParseSymbol(Symbol::LessThan).parse(state)
                {
                    let (state, generics) = expect!(
                        state,
                        OneOrMore(ParseTypeInfo, ParseSymbol(Symbol::Comma), false).parse(state),
                        "expected type parameters"
                    );

                    let (state, end_arrow) = expect!(
                        state,
                        ParseSymbol(Symbol::GreaterThan).parse(state),
                        "expected `>` to close `<`"
                    );

                    (
                        state,
                        TypeInfo::Generic {
                            base: identifier,
                            arrows: ContainedSpan::new(start_arrow, end_arrow),
                            generics,
                        },
                    )
                } else {
                    (state, TypeInfo::Basic(identifier))
                }
            } else if let Ok((state, start_parenthese)) =
                ParseSymbol(Symbol::LeftParen).parse(state)
            {
                let (state, types) = expect!(
                    state,
                    ZeroOrMoreDelimited(ParseTypeInfo, ParseSymbol(Symbol::Comma), false)
                        .parse(state),
                    "expected types within parentheses"
                );

                let (state, end_parenthese) = expect!(
                    state,
                    ParseSymbol(Symbol::RightParen).parse(state),
                    "expected `)` to match `(`"
                );

                if let Ok((state, arrow)) = ParseSymbol(Symbol::ThinArrow).parse(state) {
                    let (state, return_value) = expect!(
                        state,
                        ParseTypeInfo.parse(state),
                        "expected return type after `->`"
                    );

                    (
                        state,
                        TypeInfo::Callback {
                            arguments: types,
                            parentheses: ContainedSpan::new(start_parenthese, end_parenthese),
                            arrow,
                            return_type: Box::new(return_value),
                        },
                    )
                } else {
                    (
                        state,
                        TypeInfo::Tuple {
                            parentheses: ContainedSpan::new(start_parenthese, end_parenthese),
                            types,
                        },
                    )
                }
            } else if let Ok((state, start_brace)) = ParseSymbol(Symbol::LeftBrace).parse(state) {
                if let Ok((state, fields)) = ZeroOrMoreDelimited(ParseTypeField, ParseSymbol(Symbol::Comma), true)
                        .parse(state)
                {
                    let (state, end_brace) = expect!(
                        state,
                        ParseSymbol(Symbol::RightBrace).parse(state),
                        "expected `}` to match `{`"
                    );

                    (
                        state,
                        TypeInfo::Table {
                            braces: ContainedSpan::new(start_brace, end_brace),
                            fields,
                        },
                    )
                } else {
                    let (state, type_info) = expect!(
                        state,
                        ParseTypeInfo.parse(state),
                        "expected type in array"
                    );

                    let (state, end_brace) = expect!(
                        state,
                        ParseSymbol(Symbol::RightBrace).parse(state),
                        "expected `}` to match `{`"
                    );

                    (
                        state,
                        TypeInfo::Array {
                            braces: ContainedSpan::new(start_brace, end_brace),
                            type_info: Box::new(type_info)
                        },
                    )
                }
            } else {
                return Err(InternalAstError::NoMatch);
            };

            if let Ok((new_state, question_mark)) = ParseSymbol(Symbol::QuestionMark).parse(state) {
                base_type = TypeInfo::Optional {
                    base: Box::new(base_type),
                    question_mark,
                };

                state = new_state;
            }

            if let Ok((state, pipe)) = ParseSymbol(Symbol::Pipe).parse(state) {
                let (state, right) = expect!(
                    state,
                    ParseTypeInfo.parse(state),
                    "expected type after `|` for union type"
                );

                Ok((
                    state,
                    TypeInfo::Union {
                        left: Box::new(base_type),
                        right: Box::new(right),
                        pipe,
                    },
                ))
            } else if let Ok((state, ampersand)) = ParseSymbol(Symbol::Ampersand).parse(state) {
                let (state, right) = expect!(
                    state,
                    ParseTypeInfo.parse(state),
                    "expected type after `&` for intersection type"
                );

                Ok((
                    state,
                    TypeInfo::Intersection {
                        left: Box::new(base_type),
                        right: Box::new(right),
                        ampersand,
                    },
                ))
            } else {
                Ok((state, base_type))
            }
        });

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeField;
        define_parser!(
            ParseTypeField,
            TypeField<'a>,
            |_, state: ParserState<'a, 'b>| {
                let (state, key) = ParseTypeFieldKey.parse(state)?;

                let (state, colon) = expect!(
                    state,
                    ParseSymbol(Symbol::Colon).parse(state),
                    "expected `:` after key"
                );

                let (state, value) = expect!(
                    state,
                    ParseTypeInfo.parse(state),
                    "expected value type for key"
                );

                Ok((state, TypeField { key, colon, value }))
            }
        );

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeFieldKey;
        #[rustfmt::skip]
        define_parser!(ParseTypeFieldKey, TypeFieldKey<'a>, |_, state: ParserState<'a, 'b>| {
            if let Ok((state, identifier)) = ParseIdentifier.parse(state) {
                Ok((state, TypeFieldKey::Name(identifier)))
            } else if let Ok((state, start_bracket)) = ParseSymbol(Symbol::LeftBracket).parse(state)
            {
                let (state, inner) = expect!(
                    state,
                    ParseTypeInfo.parse(state),
                    "expected type within brackets for index signature"
                );

                let (state, end_bracket) = expect!(
                    state,
                    ParseSymbol(Symbol::RightBracket).parse(state),
                    "expected `]` to match `[`"
                );

                Ok((
                    state,
                    TypeFieldKey::IndexSignature {
                        brackets: ContainedSpan::new(start_bracket, end_bracket),
                        inner,
                    },
                ))
            } else {
                Err(InternalAstError::NoMatch)
            }
        });

        #[derive(Clone, Debug, PartialEq)]
        struct ParseTypeSpecifier;
        define_parser!(
            ParseTypeSpecifier,
            TypeSpecifier<'a>,
            |_, state: ParserState<'a, 'b>| {
                let (state, punctuation) = ParseSymbol(Symbol::Colon).parse(state)?;
                let (state, type_info) = expect!(
                    state,
                    ParseTypeInfo.parse(state),
                    "expected type after colon"
                );

                Ok((
                    state,
                    TypeSpecifier {
                        punctuation,
                        type_info,
                    },
                ))
            }
        );
    }
}

macro_rules! make_op_parser {
	($enum:ident, $parser:ident, { $($operator:ident,)+ }) => {
		#[derive(Clone, Debug, PartialEq)]
        struct $parser;
        define_parser!($parser, $enum<'a>, |_, state: ParserState<'a, 'b>| {
            $(
                if let Ok((state, operator)) = ParseSymbol(Symbol::$operator).parse(state) {
                    return Ok((state, $enum::$operator(operator)));
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

#[cfg(feature = "roblox")]
make_op_parser!(CompoundOp, ParseCompoundOp,
    {
        PlusEqual,
        MinusEqual,
        StarEqual,
        SlashEqual,
        PercentEqual,
        CaretEqual,
        TwoDotsEqual,
    }
);

// TODO

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::{ast::extract_token_references, tokenizer::tokens};
//     use pretty_assertions::assert_eq;

//     macro_rules! assert_state_eq {
//         ($state: expr, $index: expr, $tokens: ident) => {
//             assert_eq!($state.index, $index);
//             assert_eq!($state.len, $tokens.len());
//         };
//     }

//     macro_rules! tokens {
//         ($body: expr) => {
//             extract_token_references(tokens($body).expect("couldn't tokenize'"))
//         };
//     }

//     #[test]
//     fn test_zero_or_more_empty() {
//         let tokens = tokens!("local x");
//         let state = ParserState::new(&tokens);

//         let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma))
//             .parse(state)
//             .unwrap();

//         assert_state_eq!(state, 0, tokens);
//         assert_eq!(commas.len(), 0);
//     }

//     #[test]
//     fn test_zero_or_more_exists() {
//         let tokens = tokens!(",,, , ,\t ,local x");
//         let state = ParserState::new(&tokens);

//         let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma))
//             .parse(state)
//             .unwrap();

//         assert_state_eq!(state, 9, tokens);
//         assert_eq!(commas.len(), 6);
//     }

//     #[test]
//     fn test_one_or_more_empty() {
//         let tokens = tokens!("local x");
//         let state = ParserState::new(&tokens);

//         assert!(
//             OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
//                 .parse(state)
//                 .is_err()
//         );
//     }

//     #[test]
//     fn test_one_or_more_exists_no_delimiter() {
//         let tokens = tokens!("end,end, end,\t\tend local");
//         let state = ParserState::new(&tokens);

//         let (state, commas) =
//             OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
//                 .parse(state)
//                 .expect("OneOrMore failed");

//         assert_state_eq!(state, 10, tokens);
//         assert_eq!(commas.len(), 4);
//     }

//     #[test]
//     fn test_one_or_more_exists_with_delimiter() {
//         let tokens = tokens!("end,end, end,\t\tend, local");
//         let state = ParserState::new(&tokens);

//         let (state, commas) = OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
//             .parse(state)
//             .unwrap();

//         assert_state_eq!(state, 11, tokens);
//         assert_eq!(commas.len(), 4);
//     }

//     #[test]
//     fn test_one_or_more_exists_with_nothing() {
//         let tokens = tokens!("local");
//         let state = ParserState::new(&tokens);

//         assert!(
//             OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
//                 .parse(state)
//                 .is_err()
//         );
//     }
// }
