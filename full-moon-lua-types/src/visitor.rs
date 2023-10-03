use crate::{ast_traits::CreateAstNode, core, shared};
use full_moon::{
    ast,
    node::Node,
    tokenizer,
    visitors::{Visit, Visitor},
};
use mlua::UserData;
use paste::paste;

macro_rules! create_visitor {
    (
		ast: {
			$(
				($name:ident: $type:ty, $converter:expr),
			)+
		},

		tokens: {
			$(
				$token_name:ident,
			)+
		}
	) => {
		paste! {
			#[derive(Clone, Debug, Default)]
			pub struct VisitorTable<'lua> {
				$(
					$name: Option<mlua::Function<'lua>>,
					[< $name _end >]: Option<mlua::Function<'lua>>,
				)+

				$(
					$token_name: Option<mlua::Function<'lua>>,
				)+
			}

			impl<'lua> mlua::FromLua<'lua> for VisitorTable<'lua> {
				fn from_lua(value: mlua::Value<'lua>, _: &'lua mlua::Lua) -> mlua::Result<Self> {
					let mut visitor_table = VisitorTable::default();

					let table = match value {
						mlua::Value::Table(table) => table,
						_ => return Err(mlua::Error::external(format!(":visit() expects a table, received {}", value.type_name()))),
					};

					for pair in table.pairs::<mlua::String, mlua::Function<'lua>>() {
						let (key, value) = pair?;

						// TODO: When validating names, have a list of names for 5.2/5.3/Luau only that won't error when the feature is not enabled
						$(
							let pascal_cased_name = pascal_case_name(stringify!($name));

							if key == pascal_cased_name {
								visitor_table.$name = Some(value);
								continue;
							} else if key == format!("{pascal_cased_name}End") {
								visitor_table.[< $name _end >] = Some(value);
								continue;
							}
						)+

						$(
							let pascal_cased_name = pascal_case_name(stringify!($token_name));

							if key == pascal_cased_name {
								visitor_table.$token_name = Some(value);
								continue;
							}
						)+

						return Err(mlua::Error::external(format!(":visit() received an unknown key {}", key.to_string_lossy())));
					}

					Ok(visitor_table)
				}
			}

			pub struct LuaVisitor<'lua> {
				existing_error: Option<mlua::Error>,
				visitor_table: VisitorTable<'lua>,
			}

			impl<'lua> LuaVisitor<'lua> {
				fn ok(self) -> mlua::Result<()> {
					match self.existing_error {
						Some(error) => Err(error),
						None => Ok(()),
					}
				}
			}

			impl<'lua> Visitor for LuaVisitor<'lua> {
				$(
					fn $name(&mut self, node: &$type) {
						if self.existing_error.is_some() {
							return;
						}

						if let Some(function) = &self.visitor_table.$name {
							if let Err(error) = function.call::<_, ()>($converter(node)) {
								self.existing_error = Some(error);
							}
						}
					}

					fn [<$name _end>](&mut self, node: &$type) {
						if self.existing_error.is_some() {
							return;
						}

						if let Some(function) = &self.visitor_table.[< $name _end >] {
							if let Err(error) = function.call::<_, ()>($converter(node)) {
								self.existing_error = Some(error);
							}
						}
					}
				)+

				$(
					fn $token_name(&mut self, token: &tokenizer::Token) {
						if self.existing_error.is_some() {
							return;
						}

						if let Some(function) = &self.visitor_table.$token_name {
							if let Err(error) = function.call::<_, ()>(shared::Token::from(token)) {
								self.existing_error = Some(error);
							}
						}
					}
				)+
			}
		}
	};
}

create_visitor!(ast: {
    (visit_anonymous_call: ast::FunctionArgs, core::FunctionArgs::new),
    (visit_assignment: ast::Assignment, core::Assignment::new),
    (visit_block: ast::Block, core::Block::new),
    (visit_call: ast::Call, core::Call::new),
    (visit_contained_span: ast::span::ContainedSpan, shared::ContainedSpan::new),
    (visit_do: ast::Do, core::Do::new),
    (visit_else_if: ast::ElseIf, core::ElseIf::new),
    (visit_eof: tokenizer::TokenReference, shared::TokenReference::new),
    (visit_expression: ast::Expression, core::Expression::new),
    (visit_field: ast::Field, core::Field::new),
    (visit_function_args: ast::FunctionArgs, core::FunctionArgs::new),
    (visit_function_body: ast::FunctionBody, core::FunctionBody::new),
    (visit_function_call: ast::FunctionCall, core::FunctionCall::new),
    (visit_function_declaration: ast::FunctionDeclaration, core::FunctionDeclaration::new),
    (visit_function_name: ast::FunctionName, core::FunctionName::new),
    (visit_generic_for: ast::GenericFor, core::GenericFor::new),
    (visit_if: ast::If, core::If::new),
    (visit_index: ast::Index, core::Index::new),
    (visit_local_assignment: ast::LocalAssignment, core::LocalAssignment::new),
    (visit_local_function: ast::LocalFunction, core::LocalFunction::new),
    (visit_last_stmt: ast::LastStmt, core::LastStmt::new),
    (visit_method_call: ast::MethodCall, core::MethodCall::new),
    (visit_numeric_for: ast::NumericFor, core::NumericFor::new),
    (visit_parameter: ast::Parameter, core::Parameter::new),
    (visit_prefix: ast::Prefix, core::Prefix::new),
    (visit_return: ast::Return, core::Return::new),
    (visit_repeat: ast::Repeat, core::Repeat::new),
    (visit_stmt: ast::Stmt, core::Stmt::new),
    (visit_suffix: ast::Suffix, core::Suffix::new),
    (visit_table_constructor: ast::TableConstructor, core::TableConstructor::new),
    (visit_token_reference: tokenizer::TokenReference, shared::TokenReference::new),
    (visit_un_op: ast::UnOp, core::UnOp::new),
    (visit_value: ast::Value, core::Value::new),
    (visit_var: ast::Var, core::Var::new),
    (visit_var_expression: ast::VarExpression, core::VarExpression::new),
    (visit_while: ast::While, core::While::new),
}, tokens: {
    visit_identifier,
    visit_multi_line_comment,
    visit_number,
    visit_single_line_comment,
    visit_string_literal,
    visit_symbol,
    visit_token,
    visit_whitespace,
});

fn pascal_case_name(name: &str) -> String {
    let mut pascal_case_name = String::new();

    let mut should_capitalize = true;
    for character in name.chars().skip("visit_".len()) {
        if should_capitalize {
            pascal_case_name.push(character.to_ascii_uppercase());
            should_capitalize = false;
        } else if character == '_' {
            should_capitalize = true;
        } else {
            pascal_case_name.push(character);
        }
    }

    pascal_case_name
}

pub fn add_visit<'lua, T, N>(methods: &mut impl mlua::UserDataMethods<'lua, T>)
where
    T: UserData + CreateAstNode<Node = N>,
    N: Visit,
{
    methods.add_method_mut("visit", |_, this, visitor: VisitorTable| {
        let mut visitor = LuaVisitor {
            existing_error: None,
            visitor_table: visitor,
        };

        if let Some(ast_node) = this.create_ast_node() {
            ast_node.visit(&mut visitor);
        }

        visitor.ok()
    });
}

pub fn add_visit_with_visitor<'lua, T, N, F>(
    methods: &mut impl mlua::UserDataMethods<'lua, T>,
    mut callback: F,
) where
    T: UserData + Send + Sync + CreateAstNode<Node = N>,
    F: 'static + Send + FnMut(N, &mut LuaVisitor<'lua>),
{
    methods.add_method_mut("visit", move |_, this, visitor: VisitorTable| {
        let mut visitor = LuaVisitor {
            existing_error: None,
            visitor_table: visitor,
        };

        if let Some(ast_node) = this.create_ast_node() {
            callback(ast_node, &mut visitor);
        }

        visitor.ok()
    });
}
