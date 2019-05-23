use crate::ast;
use crate::tokenizer::Token;
use std::borrow::Cow;

macro_rules! create_visitor {
    (ast: {
        $($visit_name:ident => $ast_type:ident,)+
    }, token: {
        $($visit_token:ident,)+
    }) => {
        pub trait Visitor<'ast> {
            fn visit_ast(&mut self, ast: &ast::Ast<'ast>) where Self: Sized {
                ast.nodes.visit(self);
            }

            $(
                fn $visit_name(&mut self, _node: &ast::$ast_type<'ast>) { }
            )+

            $(
                fn $visit_token(&mut self, _token: &Token<'ast>) { }
            )+
        }

        pub trait VisitorMut<'ast> {
            fn visit_ast(&mut self, ast: &mut ast::Ast<'ast>) where Self: Sized {
                ast.nodes.visit_mut(self);
            }

            $(
                fn $visit_name(&mut self, _node: &mut ast::$ast_type<'ast>) { }
            )+

            $(
                fn $visit_token(&mut self, _token: &mut Token<'ast>) { }
            )+
        }
    };
}

pub trait Visit<'ast> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V);
}

pub trait VisitMut<'ast> {
    fn visit_mut<V: VisitorMut<'ast>>(&mut self, visitor: &mut V);
}

impl<'ast, T: Visit<'ast>> Visit<'ast> for Vec<T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        for item in self {
            item.visit(visitor);
        }
    }
}

impl<'ast, T: VisitMut<'ast>> VisitMut<'ast> for Vec<T> {
    fn visit_mut<V: VisitorMut<'ast>>(&mut self, visitor: &mut V) {
        for item in self {
            item.visit_mut(visitor);
        }
    }
}

impl<'ast, T: Visit<'ast>> Visit<'ast> for Option<T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        if let Some(item) = self {
            item.visit(visitor);
        }
    }
}

impl<'ast, T: VisitMut<'ast>> VisitMut<'ast> for Option<T> {
    fn visit_mut<V: VisitorMut<'ast>>(&mut self, visitor: &mut V) {
        if let Some(item) = self {
            item.visit_mut(visitor);
        }
    }
}

impl<'ast, A: Visit<'ast>, B: Visit<'ast>> Visit<'ast> for (A, B) {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        self.0.visit(visitor);
        self.1.visit(visitor);
    }
}

impl<'ast, A: VisitMut<'ast>, B: VisitMut<'ast>> VisitMut<'ast> for (A, B) {
    fn visit_mut<V: VisitorMut<'ast>>(&mut self, visitor: &mut V) {
        self.0.visit_mut(visitor);
        self.1.visit_mut(visitor);
    }
}

impl<'ast, T: Clone + Visit<'ast>> Visit<'ast> for Cow<'ast, T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<'ast, T: Clone + VisitMut<'ast>> VisitMut<'ast> for Cow<'ast, T> {
    fn visit_mut<V: VisitorMut<'ast>>(&mut self, visitor: &mut V) {
        self.to_mut().visit_mut(visitor);
    }
}

impl<'ast, T: Visit<'ast>> Visit<'ast> for Box<T> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        (**self).visit(visitor);
    }
}

impl<'ast, T: VisitMut<'ast>> VisitMut<'ast> for Box<T> {
    fn visit_mut<V: VisitorMut<'ast>>(&mut self, visitor: &mut V) {
        (**self).visit_mut(visitor);
    }
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
