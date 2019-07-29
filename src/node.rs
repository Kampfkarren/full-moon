use crate::tokenizer::{Position, TokenReference};

pub trait Node {
    fn start_position(&self) -> Option<Position>;
    fn end_position(&self) -> Option<Position>;
}

impl<'a> Node for TokenReference<'a> {
    fn start_position(&self) -> Option<Position> {
        Some((**self).start_position())
    }

    fn end_position(&self) -> Option<Position> {
        Some((**self).end_position())
    }
}

impl<T: Node> Node for Option<T> {
    fn start_position(&self) -> Option<Position> {
        self.as_ref().and_then(Node::start_position)
    }

    fn end_position(&self) -> Option<Position> {
        self.as_ref().and_then(Node::end_position)
    }
}

impl<T: Node> Node for Vec<T> {
    fn start_position(&self) -> Option<Position> {
        self.first()?.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.last()?.end_position()
    }
}

impl<A: Node, B: Node> Node for (A, B) {
    fn start_position(&self) -> Option<Position> {
        self.0.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.1.end_position()
    }
}
