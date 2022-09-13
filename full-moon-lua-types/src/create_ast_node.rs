use std::sync::Arc;

use crate::mlua_util::ArcLocked;

pub trait CreateAstNode {
    type Node;

    fn create_ast_node(&self) -> Option<Self::Node>;
}

impl<T: CreateAstNode> CreateAstNode for Box<T> {
    type Node = Box<T::Node>;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(Box::new((**self).create_ast_node()?))
    }
}

impl<T: CreateAstNode> CreateAstNode for Option<T> {
    type Node = T::Node;

    fn create_ast_node(&self) -> Option<Self::Node> {
        self.as_ref().and_then(|value| value.create_ast_node())
    }
}

impl<T: CreateAstNode> CreateAstNode for ArcLocked<T> {
    type Node = T::Node;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Arc::clone(self).read().unwrap().create_ast_node()
    }
}

impl<T: CreateAstNode, U: CreateAstNode> CreateAstNode for (T, U) {
    type Node = (T::Node, U::Node);

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some((self.0.create_ast_node()?, self.1.create_ast_node()?))
    }
}
