use crate::ast;
use crate::tokenizer::{Token, TokenType};

macro_rules! create_visitor {
    (ast: {
        $($visit_name:ident => $ast_type:ident,)+
    }, token: {
        $($visit_token:ident,)+
    }) => {
        pub trait Visitor<'ast> {
            fn visit_ast(&mut self, ast: &ast::Ast<'ast>) {
                self.visit_block(&ast.nodes);
            }

            $(
                fn $visit_name(&mut self, _node: &ast::$ast_type<'ast>) { }
            )+

            $(
                fn $visit_token(&mut self, _token: &Token<'ast>) { }
            )+
        }
    };
}

create_visitor!(ast: {
    visit_anonymous_call => FunctionArgs,
    visit_assignment => Assignment,
    visit_bin_op => BinOpRhs,
    visit_block => Block,
    visit_call => Call,
    visit_do => Block,
    visit_expression => Expression,
    visit_field => Field,
    visit_function_args => FunctionArgs,
    visit_function_body => FunctionBody,
    visit_function_call => FunctionCall,
    visit_function_declaration => FunctionDeclaration,
    visit_function_name => FunctionName,
    visit_generic_for => GenericFor,
    visit_if => If,
    visit_index => Index,
    visit_local_assignment => LocalAssignment,
    visit_local_function => LocalFunction,
    visit_last_stmt => LastStmt,
    visit_method_call => MethodCall,
    visit_numeric_for => NumericFor,
    visit_parameter => Parameter,
    visit_prefix => Prefix,
    visit_repeat => Repeat,
    visit_stmt => Stmt,
    visit_suffix => Suffix,
    visit_table_constructor => TableConstructor,
    visit_table_constructor_field => TableConstructorField,
    visit_un_op => UnOp,
    visit_value => Value,
    visit_var => Var,
    visit_var_expression => VarExpression,
    visit_while => While,
}, token: {
    visit_eof,
    visit_identifier,
    visit_multi_line_comment,
    visit_number,
    visit_single_line_comment,
    visit_string_literal,
    visit_symbol,
    visit_token,
    visit_whitespace,
});

macro_rules! interpret {
    ($self:ident.$name:ident(Option[$argument:expr])) => {{
        if let Some(argument) = $argument {
            interpret!($self.$name(argument));
        }
    }};

    ($self:ident.$name:ident($argument:expr)) => {{
        $self.$name($argument);
        ($self.0).$name($argument);
    }};
}

/// Recurses through given nodes and calls the attached visitor's associated methods
///
/// # Examples
///
/// ```
/// use full_moon::{
///     parse,
///     tokenizer::{Token, TokenType},
///     visitors::{Interpreter, Visitor},
/// };
///
/// let ast = parse("local x = 1.3").expect("couldn't parse");
///
/// struct TestVisitor(Option<String>);
/// impl<'ast> Visitor<'ast> for TestVisitor {
///     fn visit_number(&mut self, token: &Token<'ast>) {
///         match &token.token_type {
///             TokenType::Number { text } => {
///                 self.0 = Some(text.to_string());
///             }
///
///             _ => unreachable!(),
///         }
///     }
/// }
///
/// let mut test_visitor = TestVisitor(None);
/// Interpreter(&mut test_visitor).visit_ast(&ast);
/// assert_eq!(test_visitor.0, Some("1.3".to_string()));
/// ```
pub struct Interpreter<'ast>(pub &'ast mut Visitor<'ast>);
impl<'ast> Visitor<'ast> for Interpreter<'ast> {
    fn visit_assignment(&mut self, assignment: &ast::Assignment<'ast>) {
        for var in &assignment.var_list {
            interpret!(self.visit_var(&var));
        }

        for expr in &assignment.expr_list {
            interpret!(self.visit_expression(&expr));
        }
    }

    fn visit_bin_op(&mut self, rhs: &ast::BinOpRhs<'ast>) {
        interpret!(self.visit_expression(&rhs.1));
    }

    fn visit_block(&mut self, block: &ast::Block<'ast>) {
        for stmt in &block.stmts {
            interpret!(self.visit_stmt(&stmt));
        }

        interpret!(self.visit_last_stmt(Option[&block.last_stmt]));
    }

    fn visit_call(&mut self, call: &ast::Call<'ast>) {
        match call {
            ast::Call::AnonymousCall(args) => {
                interpret!(self.visit_anonymous_call(args));
                interpret!(self.visit_function_args(args));
            }

            ast::Call::MethodCall(method_call) => interpret!(self.visit_method_call(method_call)),
        }
    }

    fn visit_expression(&mut self, expression: &ast::Expression<'ast>) {
        match expression {
            ast::Expression::UnaryOperator { unop, expression } => {
                interpret!(self.visit_un_op(unop));
                interpret!(self.visit_expression(expression));
            }

            ast::Expression::Value { value, binop } => {
                interpret!(self.visit_value(value));
                interpret!(self.visit_bin_op(Option[binop]));
            }
        }
    }

    fn visit_field(&mut self, field: &ast::Field<'ast>) {
        match field {
            ast::Field::ExpressionKey { key, value } => {
                interpret!(self.visit_expression(&key));
                interpret!(self.visit_expression(&value));
            }

            ast::Field::NameKey { key, value } => {
                interpret!(self.visit_token(&key));
                interpret!(self.visit_expression(&value));
            }

            ast::Field::NoKey(value) => interpret!(self.visit_expression(value)),
        }
    }

    fn visit_function_args(&mut self, function_args: &ast::FunctionArgs<'ast>) {
        match function_args {
            ast::FunctionArgs::Parentheses(arguments) => {
                for argument in arguments {
                    interpret!(self.visit_expression(&argument));
                }
            }
            ast::FunctionArgs::String(token) => interpret!(self.visit_token(&token)),
            ast::FunctionArgs::TableConstructor(table) => {
                interpret!(self.visit_table_constructor(&table))
            }
        }
    }

    fn visit_function_body(&mut self, function_body: &ast::FunctionBody<'ast>) {
        for parameter in &function_body.parameters {
            interpret!(self.visit_parameter(&parameter));
        }

        interpret!(self.visit_block(&function_body.block));
    }

    fn visit_function_call(&mut self, function_call: &ast::FunctionCall<'ast>) {
        interpret!(self.visit_prefix(&function_call.prefix));

        for suffix in &function_call.suffixes {
            interpret!(self.visit_suffix(&suffix));
        }
    }

    fn visit_function_declaration(
        &mut self,
        function_declaration: &ast::FunctionDeclaration<'ast>,
    ) {
        interpret!(self.visit_function_name(&function_declaration.name));
        interpret!(self.visit_function_body(&function_declaration.body));
    }

    fn visit_function_name(&mut self, function_name: &ast::FunctionName<'ast>) {
        for name in &function_name.names {
            interpret!(self.visit_token(&name));
        }

        interpret!(self.visit_token(Option[&function_name.colon_name]));
    }

    fn visit_generic_for(&mut self, generic_for: &ast::GenericFor<'ast>) {
        for name in &generic_for.names {
            interpret!(self.visit_token(&name));
        }

        for expr in &generic_for.expr_list {
            interpret!(self.visit_expression(&expr));
        }

        interpret!(self.visit_block(&generic_for.block));
    }

    fn visit_if(&mut self, if_stmt: &ast::If<'ast>) {
        interpret!(self.visit_expression(&if_stmt.condition));
        interpret!(self.visit_block(&if_stmt.block));
        if let Some(else_ifs) = &if_stmt.else_if {
            for else_if in else_ifs {
                interpret!(self.visit_expression(&else_if.0));
                interpret!(self.visit_block(&else_if.1));
            }
        }
        interpret!(self.visit_block(Option[&if_stmt.r#else]))
    }

    fn visit_last_stmt(&mut self, last_stmt: &ast::LastStmt<'ast>) {
        if let ast::LastStmt::Return(expressions) = last_stmt {
            for expression in expressions {
                interpret!(self.visit_expression(expression));
            }
        }
    }

    fn visit_local_assignment(&mut self, local_assignment: &ast::LocalAssignment<'ast>) {
        for name in &local_assignment.name_list {
            interpret!(self.visit_token(&name));
        }

        for expression in &local_assignment.expr_list {
            interpret!(self.visit_expression(&expression));
        }
    }

    fn visit_local_function(&mut self, local_function: &ast::LocalFunction<'ast>) {
        interpret!(self.visit_token(&local_function.name));
        interpret!(self.visit_function_body(&local_function.func_body));
    }

    fn visit_method_call(&mut self, method_call: &ast::MethodCall<'ast>) {
        interpret!(self.visit_token(&method_call.name));
        interpret!(self.visit_function_args(&method_call.args));
    }

    fn visit_numeric_for(&mut self, numeric_for: &ast::NumericFor<'ast>) {
        interpret!(self.visit_token(&numeric_for.index_variable));
        interpret!(self.visit_expression(&numeric_for.start));
        interpret!(self.visit_expression(&numeric_for.end));
        interpret!(self.visit_expression(Option[&numeric_for.step]));
        interpret!(self.visit_block(&numeric_for.block));
    }

    fn visit_table_constructor(&mut self, table: &ast::TableConstructor<'ast>) {
        for field in &table.fields {
            interpret!(self.visit_table_constructor_field(&field));
        }
    }

    fn visit_table_constructor_field(&mut self, field: &ast::TableConstructorField<'ast>) {
        interpret!(self.visit_field(&field.0));
        interpret!(self.visit_token(Option[&field.1]));
    }

    fn visit_token(&mut self, token: &Token<'ast>) {
        match token.token_type {
            TokenType::Eof => interpret!(self.visit_eof(token)),
            TokenType::Identifier { .. } => interpret!(self.visit_identifier(token)),
            TokenType::MultiLineComment { .. } => interpret!(self.visit_multi_line_comment(token)),
            TokenType::Number { .. } => interpret!(self.visit_number(token)),
            TokenType::SingleLineComment { .. } => {
                interpret!(self.visit_single_line_comment(token))
            }
            TokenType::StringLiteral { .. } => interpret!(self.visit_string_literal(token)),
            TokenType::Symbol { .. } => interpret!(self.visit_symbol(token)),
            TokenType::Whitespace { .. } => interpret!(self.visit_whitespace(token)),
        }
    }

    fn visit_parameter(&mut self, parameter: &ast::Parameter<'ast>) {
        match parameter {
            ast::Parameter::Ellipse(token) | ast::Parameter::Name(token) => {
                interpret!(self.visit_token(&token))
            }
        }
    }

    fn visit_prefix(&mut self, prefix: &ast::Prefix<'ast>) {
        match prefix {
            ast::Prefix::Expression(expression) => interpret!(self.visit_expression(expression)),
            ast::Prefix::Name(name) => interpret!(self.visit_token(name)),
        }
    }

    fn visit_repeat(&mut self, repeat: &ast::Repeat<'ast>) {
        interpret!(self.visit_block(&repeat.block));
        interpret!(self.visit_expression(&repeat.until));
    }

    fn visit_stmt(&mut self, stmt: &ast::Stmt<'ast>) {
        match stmt {
            ast::Stmt::Assignment(assignment) => interpret!(self.visit_assignment(assignment)),
            ast::Stmt::Do(block) => {
                interpret!(self.visit_do(block));
                interpret!(self.visit_block(block));
            }
            ast::Stmt::FunctionCall(call) => interpret!(self.visit_function_call(call)),
            ast::Stmt::FunctionDeclaration(decl) => {
                interpret!(self.visit_function_declaration(decl))
            }
            ast::Stmt::GenericFor(generic_for) => interpret!(self.visit_generic_for(generic_for)),
            ast::Stmt::If(if_stmt) => interpret!(self.visit_if(if_stmt)),
            ast::Stmt::LocalAssignment(local_assignment) => {
                interpret!(self.visit_local_assignment(local_assignment))
            }
            ast::Stmt::LocalFunction(local_function) => {
                interpret!(self.visit_local_function(local_function))
            }
            ast::Stmt::NumericFor(numeric_for) => interpret!(self.visit_numeric_for(numeric_for)),
            ast::Stmt::Repeat(repeat) => interpret!(self.visit_repeat(repeat)),
            ast::Stmt::While(while_loop) => interpret!(self.visit_while(while_loop)),
        }
    }

    fn visit_suffix(&mut self, suffix: &ast::Suffix<'ast>) {
        match suffix {
            ast::Suffix::Call(call) => interpret!(self.visit_call(call)),
            ast::Suffix::Index(index) => interpret!(self.visit_index(index)),
        }
    }

    fn visit_value(&mut self, value: &ast::Value<'ast>) {
        match value {
            ast::Value::Function(function) => interpret!(self.visit_function_body(function)),
            ast::Value::FunctionCall(call) => interpret!(self.visit_function_call(call)),
            ast::Value::Number(token) => interpret!(self.visit_token(token)),
            ast::Value::ParseExpression(expression) => {
                interpret!(self.visit_expression(expression))
            }
            ast::Value::String(token) => interpret!(self.visit_string_literal(token)),
            ast::Value::Symbol(token) => interpret!(self.visit_symbol(token)),
            ast::Value::TableConstructor(table) => interpret!(self.visit_table_constructor(table)),
            ast::Value::Var(var) => interpret!(self.visit_var(var)),
        }
    }

    fn visit_var(&mut self, var: &ast::Var<'ast>) {
        match var {
            ast::Var::Expression(expression) => interpret!(self.visit_var_expression(expression)),
            ast::Var::Name(name) => interpret!(self.visit_token(name)),
        }
    }

    fn visit_var_expression(&mut self, var_expression: &ast::VarExpression<'ast>) {
        interpret!(self.visit_prefix(&var_expression.prefix));

        for suffix in &var_expression.suffixes {
            interpret!(self.visit_suffix(&suffix));
        }
    }

    fn visit_while(&mut self, while_loop: &ast::While<'ast>) {
        interpret!(self.visit_expression(&while_loop.condition));
        interpret!(self.visit_block(&while_loop.block));
    }
}
