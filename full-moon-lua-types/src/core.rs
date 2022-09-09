use full_moon::{ast, node::Node, tokenizer};
use mlua::UserData;

use crate::{mlua_util::add_to_string_display, shared::*};

pub struct Ast {
    nodes: Block,
    eof: TokenReference,
}

impl From<ast::Ast> for Ast {
    fn from(ast: ast::Ast) -> Self {
        Ast {
            nodes: Block::new(ast.nodes()),
            eof: TokenReference::new(ast.eof()),
        }
    }
}

impl UserData for Ast {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        // fields.add_field_method_get("nodes", |_, Ast { nodes, .. }| Ok(nodes))
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Ast", methods);
    }
}

pub struct Assignment {
    var_list: Punctuated<Var>,
    equal_token: TokenReference,
    expr_list: Punctuated<Expression>,
}

impl Assignment {
    fn new(assignment: &ast::Assignment) -> Self {
        Assignment {
            var_list: Punctuated::map_from_punctuated(assignment.variables(), Var::new),
            equal_token: TokenReference::new(assignment.equal_token()),
            expr_list: Punctuated::map_from_punctuated(assignment.expressions(), Expression::new),
        }
    }
}

impl UserData for Assignment {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Assignment", methods);
    }
}

pub enum BinOp {
    And(TokenReference),
    Caret(TokenReference),
    GreaterThan(TokenReference),
    GreaterThanEqual(TokenReference),
    LessThan(TokenReference),
    LessThanEqual(TokenReference),
    Minus(TokenReference),
    Or(TokenReference),
    Percent(TokenReference),
    Plus(TokenReference),
    Slash(TokenReference),
    Star(TokenReference),
    TildeEqual(TokenReference),
    TwoDots(TokenReference),
    TwoEqual(TokenReference),
}

impl BinOp {
    fn new(bin_op: &ast::BinOp) -> Self {
        match bin_op {
            ast::BinOp::And(token) => BinOp::And(TokenReference::new(&token)),
            ast::BinOp::Caret(token) => BinOp::Caret(TokenReference::new(&token)),
            ast::BinOp::GreaterThan(token) => BinOp::GreaterThan(TokenReference::new(&token)),
            ast::BinOp::GreaterThanEqual(token) => {
                BinOp::GreaterThanEqual(TokenReference::new(&token))
            }
            ast::BinOp::LessThan(token) => BinOp::LessThan(TokenReference::new(&token)),
            ast::BinOp::LessThanEqual(token) => BinOp::LessThanEqual(TokenReference::new(&token)),
            ast::BinOp::Minus(token) => BinOp::Minus(TokenReference::new(&token)),
            ast::BinOp::Or(token) => BinOp::Or(TokenReference::new(&token)),
            ast::BinOp::Percent(token) => BinOp::Percent(TokenReference::new(&token)),
            ast::BinOp::Plus(token) => BinOp::Plus(TokenReference::new(&token)),
            ast::BinOp::Slash(token) => BinOp::Slash(TokenReference::new(&token)),
            ast::BinOp::Star(token) => BinOp::Star(TokenReference::new(&token)),
            ast::BinOp::TildeEqual(token) => BinOp::TildeEqual(TokenReference::new(&token)),
            ast::BinOp::TwoDots(token) => BinOp::TwoDots(TokenReference::new(&token)),
            ast::BinOp::TwoEqual(token) => BinOp::TwoEqual(TokenReference::new(&token)),
            other => panic!("unimplemented BinOp: {other:?}"),
        }
    }
}

impl UserData for BinOp {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("BinOp", methods);
    }
}

pub struct Block {
    stmts: Vec<(Stmt, Option<TokenReference>)>,
    last_stmt: Option<(LastStmt, Option<TokenReference>)>,
}

impl Block {
    fn new(block: &ast::Block) -> Self {
        Block {
            stmts: block
                .stmts_with_semicolon()
                .map(|(stmt, token)| (Stmt::new(stmt), token.as_ref().map(TokenReference::new)))
                .collect(),

            last_stmt: block
                .last_stmt_with_semicolon()
                .as_ref()
                .map(|(last_stmt, token)| {
                    (
                        LastStmt::new(last_stmt),
                        token.as_ref().map(TokenReference::new),
                    )
                }),
        }
    }
}

impl UserData for Block {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Block", methods);
    }
}

pub enum Call {
    AnonymousCall(FunctionArgs),
    MethodCall(MethodCall),
}

impl Call {
    fn new(call: &ast::Call) -> Self {
        match call {
            ast::Call::AnonymousCall(function_args) => {
                Call::AnonymousCall(FunctionArgs::new(function_args))
            }
            ast::Call::MethodCall(method_call) => Call::MethodCall(MethodCall::new(method_call)),
            other => panic!("unimplemented Call: {other:?}"),
        }
    }
}

impl UserData for Call {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Call", methods);
    }
}

pub struct Do {
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl Do {
    fn new(do_: &ast::Do) -> Self {
        Do {
            do_token: TokenReference::new(do_.do_token()),
            block: Block::new(do_.block()),
            end_token: TokenReference::new(do_.end_token()),
        }
    }
}

impl UserData for Do {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Do", methods);
    }
}

pub struct ElseIf {
    else_if_token: TokenReference,
    condition: Expression,
    then_token: TokenReference,
    block: Block,
}

impl ElseIf {
    fn new(else_if: &ast::ElseIf) -> Self {
        ElseIf {
            else_if_token: TokenReference::new(else_if.else_if_token()),
            condition: Expression::new(else_if.condition()),
            then_token: TokenReference::new(else_if.then_token()),
            block: Block::new(else_if.block()),
        }
    }
}

impl UserData for ElseIf {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("ElseIf", methods);
    }
}

pub enum Expression {
    BinaryOperator {
        lhs: Box<Expression>,
        binop: BinOp,
        rhs: Box<Expression>,
    },

    Parentheses {
        contained: ContainedSpan,
        expression: Box<Expression>,
    },

    UnaryOperator {
        unop: UnOp,
        expression: Box<Expression>,
    },

    Value {
        value: Box<Value>,
    },
}

impl Expression {
    fn new(expression: &ast::Expression) -> Self {
        match expression {
            ast::Expression::BinaryOperator { lhs, binop, rhs } => Expression::BinaryOperator {
                lhs: Box::new(Expression::new(lhs)),
                binop: BinOp::new(binop),
                rhs: Box::new(Expression::new(rhs)),
            },

            ast::Expression::Parentheses {
                contained,
                expression,
            } => Expression::Parentheses {
                contained: ContainedSpan::new(contained),
                expression: Box::new(Expression::new(expression)),
            },

            ast::Expression::UnaryOperator { unop, expression } => Expression::UnaryOperator {
                unop: UnOp::new(unop),
                expression: Box::new(Expression::new(expression)),
            },

            ast::Expression::Value { value } => Expression::Value {
                value: Box::new(Value::new(value)),
            },

            other => panic!("unimplemented Expression: {other:?}"),
        }
    }
}

impl UserData for Expression {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Expression", methods);
    }
}

pub enum FunctionArgs {
    Parentheses {
        contained: ContainedSpan,
        expressions: Punctuated<Expression>,
    },

    String(TokenReference),

    TableConstructor(TableConstructor),
}

impl FunctionArgs {
    fn new(function_args: &ast::FunctionArgs) -> Self {
        match function_args {
            ast::FunctionArgs::Parentheses {
                parentheses,
                arguments,
            } => FunctionArgs::Parentheses {
                contained: ContainedSpan::new(parentheses),
                expressions: Punctuated::map_from_punctuated(arguments, Expression::new),
            },

            ast::FunctionArgs::String(token) => FunctionArgs::String(TokenReference::new(token)),

            ast::FunctionArgs::TableConstructor(table_constructor) => {
                FunctionArgs::TableConstructor(TableConstructor::new(table_constructor))
            }

            other => panic!("unimplemented FunctionArgs: {other:?}"),
        }
    }
}

impl UserData for FunctionArgs {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionArgs", methods);
    }
}

pub struct FunctionBody {
    parameters_parentheses: ContainedSpan,
    parameters: Punctuated<Parameter>,
    block: Block,
    end_token: TokenReference,
}

impl FunctionBody {
    fn new(function_body: &ast::FunctionBody) -> Self {
        FunctionBody {
            parameters_parentheses: ContainedSpan::new(function_body.parameters_parentheses()),

            parameters: Punctuated::map_from_punctuated(function_body.parameters(), Parameter::new),

            block: Block::new(function_body.block()),
            end_token: TokenReference::new(function_body.end_token()),
        }
    }
}

impl UserData for FunctionBody {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionBody", methods);
    }
}

pub enum LastStmt {
    Break(TokenReference),
    #[cfg(feature = "luau")]
    Continue(TokenReference),
    Return(Return),
}

impl LastStmt {
    fn new(last_stmt: &ast::LastStmt) -> Self {
        match last_stmt {
            ast::LastStmt::Break(break_token) => LastStmt::Break(TokenReference::new(break_token)),

            #[cfg(feature = "luau")]
            ast::LastStmt::Continue(continue_token) => {
                LastStmt::Continue(TokenReference::new(continue_token))
            }

            ast::LastStmt::Return(return_token) => LastStmt::Return(Return::new(return_token)),

            _ => unimplemented!("unexpected LastStmt variant: {last_stmt:#?}"),
        }
    }
}

impl UserData for LastStmt {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("LastStmt", methods);
    }
}

pub enum Field {
    ExpressionKey {
        brackets: ContainedSpan,
        key: Expression,
        equal: TokenReference,
        value: Expression,
    },

    NameKey {
        key: TokenReference,
        equal: TokenReference,
        value: Expression,
    },

    NoKey(Expression),
}

impl Field {
    fn new(field: &ast::Field) -> Self {
        match field {
            ast::Field::ExpressionKey {
                brackets,
                key,
                equal,
                value,
            } => Field::ExpressionKey {
                brackets: ContainedSpan::new(brackets),
                key: Expression::new(key),
                equal: TokenReference::new(equal),
                value: Expression::new(value),
            },

            ast::Field::NameKey { key, equal, value } => Field::NameKey {
                key: TokenReference::new(key),
                equal: TokenReference::new(equal),
                value: Expression::new(value),
            },

            ast::Field::NoKey(expression) => Field::NoKey(Expression::new(expression)),

            other => panic!("unimplemented Field: {other:?}"),
        }
    }
}

impl UserData for Field {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Field", methods);
    }
}

pub struct FunctionCall {
    prefix: Prefix,
    suffixes: Vec<Suffix>,
}

impl FunctionCall {
    fn new(function_call: &ast::FunctionCall) -> Self {
        FunctionCall {
            prefix: Prefix::new(function_call.prefix()),
            suffixes: function_call.suffixes().map(Suffix::new).collect(),
        }
    }
}

impl UserData for FunctionCall {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionCall", methods);
    }
}

pub struct FunctionDeclaration {
    function_token: TokenReference,
    name: FunctionName,
    body: FunctionBody,
}

impl FunctionDeclaration {
    fn new(function_declaration: &ast::FunctionDeclaration) -> Self {
        FunctionDeclaration {
            function_token: TokenReference::new(function_declaration.function_token()),
            name: FunctionName::new(function_declaration.name()),
            body: FunctionBody::new(function_declaration.body()),
        }
    }
}

impl UserData for FunctionDeclaration {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionDeclaration", methods);
    }
}

pub struct FunctionName {
    names: Punctuated<TokenReference>,
    colon_name: Option<(TokenReference, TokenReference)>,
}

impl FunctionName {
    fn new(function_name: &ast::FunctionName) -> Self {
        FunctionName {
            names: Punctuated::map_from_punctuated(function_name.names(), TokenReference::new),

            colon_name: match (function_name.method_colon(), function_name.method_name()) {
                (Some(colon), Some(name)) => {
                    Some((TokenReference::new(colon), TokenReference::new(name)))
                }

                _ => None,
            },
        }
    }
}

impl UserData for FunctionName {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionName", methods);
    }
}

pub struct GenericFor {
    for_token: TokenReference,
    names: Punctuated<TokenReference>,
    in_token: TokenReference,
    expr_list: Punctuated<Expression>,
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl GenericFor {
    fn new(generic_for: &ast::GenericFor) -> Self {
        GenericFor {
            for_token: TokenReference::new(generic_for.for_token()),
            names: Punctuated::map_from_punctuated(generic_for.names(), TokenReference::new),
            in_token: TokenReference::new(generic_for.in_token()),
            expr_list: Punctuated::map_from_punctuated(generic_for.expressions(), Expression::new),
            do_token: TokenReference::new(generic_for.do_token()),
            block: Block::new(generic_for.block()),
            end_token: TokenReference::new(generic_for.end_token()),
        }
    }
}

impl UserData for GenericFor {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("GenericFor", methods);
    }
}

pub struct If {
    if_token: TokenReference,
    condition: Expression,
    then_token: TokenReference,
    block: Block,
    else_if: Option<Vec<ElseIf>>,
    else_token: Option<TokenReference>,
    else_block: Option<Block>,
    end_token: TokenReference,
}

impl If {
    fn new(if_node: &ast::If) -> Self {
        If {
            if_token: TokenReference::new(if_node.if_token()),
            condition: Expression::new(if_node.condition()),
            then_token: TokenReference::new(if_node.then_token()),
            block: Block::new(if_node.block()),
            else_if: if_node
                .else_if()
                .map(|else_if| else_if.iter().map(ElseIf::new).collect()),
            else_token: if_node.else_token().map(TokenReference::new),
            else_block: if_node.else_block().map(Block::new),
            end_token: TokenReference::new(if_node.end_token()),
        }
    }
}

impl UserData for If {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("If", methods);
    }
}

pub enum Index {
    Brackets {
        brackets: ContainedSpan,
        expression: Expression,
    },

    Dot {
        dot: TokenReference,
        name: TokenReference,
    },
}

impl Index {
    fn new(index: &ast::Index) -> Self {
        match index {
            ast::Index::Brackets {
                brackets,
                expression,
            } => Index::Brackets {
                brackets: ContainedSpan::new(brackets),
                expression: Expression::new(expression),
            },

            ast::Index::Dot { dot, name } => Index::Dot {
                dot: TokenReference::new(dot),
                name: TokenReference::new(name),
            },

            other => panic!("unimplemented Index: {other:?}"),
        }
    }
}

impl UserData for Index {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Index", methods);
    }
}

pub struct LocalAssignment {
    local_token: TokenReference,
    name_list: Punctuated<TokenReference>,
    equal_token: Option<TokenReference>,
    expr_list: Punctuated<Expression>,
}

impl LocalAssignment {
    fn new(local_assignment: &ast::LocalAssignment) -> Self {
        LocalAssignment {
            local_token: TokenReference::new(local_assignment.local_token()),
            name_list: Punctuated::map_from_punctuated(
                local_assignment.names(),
                TokenReference::new,
            ),
            equal_token: local_assignment.equal_token().map(TokenReference::new),
            expr_list: Punctuated::map_from_punctuated(
                local_assignment.expressions(),
                Expression::new,
            ),
        }
    }
}

impl UserData for LocalAssignment {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("LocalAssignment", methods);
    }
}

pub struct LocalFunction {
    local_token: TokenReference,
    function_token: TokenReference,
    name: TokenReference,
    body: FunctionBody,
}

impl LocalFunction {
    fn new(local_function: &ast::LocalFunction) -> Self {
        LocalFunction {
            local_token: TokenReference::new(local_function.local_token()),
            function_token: TokenReference::new(local_function.function_token()),
            name: TokenReference::new(local_function.name()),
            body: FunctionBody::new(local_function.body()),
        }
    }
}

impl UserData for LocalFunction {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("LocalFunction", methods);
    }
}

pub struct MethodCall {
    colon_token: TokenReference,
    name: TokenReference,
    args: FunctionArgs,
}

impl MethodCall {
    fn new(method_call: &ast::MethodCall) -> Self {
        MethodCall {
            colon_token: TokenReference::new(method_call.colon_token()),
            name: TokenReference::new(method_call.name()),
            args: FunctionArgs::new(method_call.args()),
        }
    }
}

impl UserData for MethodCall {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("MethodCall", methods);
    }
}

pub struct NumericFor {
    for_token: TokenReference,
    index_variable: TokenReference,
    equal_token: TokenReference,
    start: Expression,
    start_end_comma: TokenReference,
    end: Expression,
    end_step_comma: Option<TokenReference>,
    step: Option<Expression>,
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl NumericFor {
    fn new(numeric_for: &ast::NumericFor) -> Self {
        NumericFor {
            for_token: TokenReference::new(numeric_for.for_token()),
            index_variable: TokenReference::new(numeric_for.index_variable()),
            equal_token: TokenReference::new(numeric_for.equal_token()),
            start: Expression::new(numeric_for.start()),
            start_end_comma: TokenReference::new(numeric_for.start_end_comma()),
            end: Expression::new(numeric_for.end()),
            end_step_comma: numeric_for.end_step_comma().map(TokenReference::new),
            step: numeric_for.step().map(Expression::new),
            do_token: TokenReference::new(numeric_for.do_token()),
            block: Block::new(numeric_for.block()),
            end_token: TokenReference::new(numeric_for.end_token()),
        }
    }
}

impl UserData for NumericFor {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("NumericFor", methods);
    }
}

pub enum Parameter {
    Ellipse(TokenReference),
    Name(TokenReference),
}

impl Parameter {
    fn new(parameter: &ast::Parameter) -> Self {
        match parameter {
            ast::Parameter::Ellipse(ellipse_token) => {
                Parameter::Ellipse(TokenReference::new(ellipse_token))
            }

            ast::Parameter::Name(name_token) => Parameter::Name(TokenReference::new(name_token)),

            _ => unimplemented!("unexpected Parameter variant: {parameter:#?}"),
        }
    }
}

impl UserData for Parameter {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Parameter", methods);
    }
}

pub enum Prefix {
    Expression(Expression),
    Name(TokenReference),
}

impl Prefix {
    fn new(prefix: &ast::Prefix) -> Self {
        match prefix {
            ast::Prefix::Expression(expr) => Prefix::Expression(Expression::new(expr)),
            ast::Prefix::Name(name) => Prefix::Name(TokenReference::new(name)),
            other => unimplemented!("unexpected Prefix variant: {other:?}"),
        }
    }
}

impl UserData for Prefix {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Prefix", methods);
    }
}

pub struct Return {
    token: TokenReference,
    returns: Punctuated<Expression>,
}

impl Return {
    fn new(return_token: &ast::Return) -> Self {
        Return {
            token: TokenReference::new(return_token.token()),
            returns: Punctuated::map_from_punctuated(return_token.returns(), Expression::new),
        }
    }
}

impl UserData for Return {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Return", methods);
    }
}

pub struct Repeat {
    repeat_token: TokenReference,
    block: Block,
    until_token: TokenReference,
    until: Expression,
}

impl Repeat {
    fn new(repeat: &ast::Repeat) -> Self {
        Repeat {
            repeat_token: TokenReference::new(repeat.repeat_token()),
            block: Block::new(repeat.block()),
            until_token: TokenReference::new(repeat.until_token()),
            until: Expression::new(repeat.until()),
        }
    }
}

impl UserData for Repeat {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Repeat", methods);
    }
}

pub enum Stmt {
    Assignment(Assignment),
    Do(Do),
    FunctionCall(FunctionCall),
    FunctionDeclaration(FunctionDeclaration),
    GenericFor(GenericFor),
    If(If),
    LocalAssignment(LocalAssignment),
    LocalFunction(LocalFunction),
    NumericFor(NumericFor),
    Repeat(Repeat),
    While(While),

    NonStandard(Vec<TokenReference>),
}

impl Stmt {
    fn new(stmt: &ast::Stmt) -> Self {
        match stmt {
            ast::Stmt::Assignment(assignment) => Stmt::Assignment(Assignment::new(assignment)),
            ast::Stmt::Do(do_token) => Stmt::Do(Do::new(do_token)),
            ast::Stmt::FunctionCall(function_call) => {
                Stmt::FunctionCall(FunctionCall::new(function_call))
            }
            ast::Stmt::FunctionDeclaration(function_declaration) => {
                Stmt::FunctionDeclaration(FunctionDeclaration::new(function_declaration))
            }
            ast::Stmt::GenericFor(generic_for) => Stmt::GenericFor(GenericFor::new(generic_for)),
            ast::Stmt::If(if_token) => Stmt::If(If::new(if_token)),
            ast::Stmt::LocalAssignment(local_assignment) => {
                Stmt::LocalAssignment(LocalAssignment::new(local_assignment))
            }
            ast::Stmt::LocalFunction(local_function) => {
                Stmt::LocalFunction(LocalFunction::new(local_function))
            }
            ast::Stmt::NumericFor(numeric_for) => Stmt::NumericFor(NumericFor::new(numeric_for)),
            ast::Stmt::Repeat(repeat_token) => Stmt::Repeat(Repeat::new(repeat_token)),
            ast::Stmt::While(while_token) => Stmt::While(While::new(while_token)),

            // TODO: Support everything, then make this `unimplemented!`
            _ => Stmt::NonStandard(
                stmt.tokens()
                    .map(|token| TokenReference::new(token))
                    .collect(),
            ),
        }
    }
}

impl UserData for Stmt {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Stmt", methods);
    }
}

pub enum Suffix {
    Call(Call),
    Index(Index),
}

impl Suffix {
    fn new(suffix: &ast::Suffix) -> Self {
        match suffix {
            ast::Suffix::Call(call) => Suffix::Call(Call::new(call)),
            ast::Suffix::Index(index) => Suffix::Index(Index::new(index)),
            other => unimplemented!("unexpected Suffix variant: {other:#?}"),
        }
    }
}

impl UserData for Suffix {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Suffix", methods);
    }
}

pub struct TableConstructor {
    braces: ContainedSpan,
    fields: Punctuated<Field>,
}

impl TableConstructor {
    fn new(table_constructor: &ast::TableConstructor) -> Self {
        TableConstructor {
            braces: ContainedSpan::new(table_constructor.braces()),
            fields: Punctuated::map_from_punctuated(table_constructor.fields(), Field::new),
        }
    }
}

impl UserData for TableConstructor {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("TableConstructor", methods);
    }
}

pub enum UnOp {
    Minus(TokenReference),
    Not(TokenReference),
    Hash(TokenReference),
}

impl UnOp {
    fn new(unop: &ast::UnOp) -> Self {
        match unop {
            ast::UnOp::Minus(token) => UnOp::Minus(TokenReference::new(token)),
            ast::UnOp::Not(token) => UnOp::Not(TokenReference::new(token)),
            ast::UnOp::Hash(token) => UnOp::Hash(TokenReference::new(token)),
            other => panic!("unimplemented UnOp: {other:?}"),
        }
    }
}

impl UserData for UnOp {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("UnOp", methods);
    }
}

pub enum Value {
    Function((TokenReference, FunctionBody)),
    FunctionCall(FunctionCall),
    TableConstructor(TableConstructor),
    Number(TokenReference),
    ParenthesesExpression(Expression),
    String(TokenReference),
    Symbol(TokenReference),
    Var(Var),

    NonStandard(Vec<TokenReference>),
}

impl Value {
    pub fn new(value: &ast::Value) -> Self {
        match value {
            ast::Value::Function((token_reference, function_body)) => Value::Function((
                TokenReference::new(token_reference),
                FunctionBody::new(function_body),
            )),

            ast::Value::FunctionCall(function_call) => {
                Value::FunctionCall(FunctionCall::new(function_call))
            }

            ast::Value::TableConstructor(table_constructor) => {
                Value::TableConstructor(TableConstructor::new(table_constructor))
            }

            ast::Value::Number(number) => Value::Number(TokenReference::new(number)),
            ast::Value::ParenthesesExpression(expression) => {
                Value::ParenthesesExpression(Expression::new(expression))
            }
            ast::Value::String(string) => Value::String(TokenReference::new(string)),
            ast::Value::Symbol(symbol) => Value::Symbol(TokenReference::new(symbol)),
            ast::Value::Var(var) => Value::Var(Var::new(var)),

            // TODO: implement everything, then `unimplemented!`
            other => Value::NonStandard(
                other
                    .tokens()
                    .map(|token| TokenReference::new(token))
                    .collect(),
            ),
        }
    }
}

impl UserData for Value {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Value", methods);
    }
}

pub enum Var {
    Expression(VarExpression),
    Name(TokenReference),
}

impl Var {
    fn new(var: &ast::Var) -> Self {
        match var {
            ast::Var::Expression(expression) => Var::Expression(VarExpression::new(expression)),
            ast::Var::Name(name_token) => Var::Name(TokenReference::new(name_token)),
            other => unimplemented!("unexpected Var variant: {var:#?}"),
        }
    }
}

impl UserData for Var {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Var", methods);
    }
}

pub struct VarExpression {
    prefix: Prefix,
    suffixes: Vec<Suffix>,
}

impl VarExpression {
    fn new(var_expression: &ast::VarExpression) -> Self {
        VarExpression {
            prefix: Prefix::new(var_expression.prefix()),
            suffixes: var_expression
                .suffixes()
                .map(|suffix| Suffix::new(suffix))
                .collect(),
        }
    }
}

impl UserData for VarExpression {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("VarExpression", methods);
    }
}

pub struct While {
    while_token: TokenReference,
    condition: Expression,
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl While {
    fn new(while_token: &ast::While) -> Self {
        While {
            while_token: TokenReference::new(while_token.while_token()),
            condition: Expression::new(while_token.condition()),
            do_token: TokenReference::new(while_token.do_token()),
            block: Block::new(while_token.block()),
            end_token: TokenReference::new(while_token.end_token()),
        }
    }
}

impl UserData for While {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("While", methods);
    }
}
