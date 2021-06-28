// Implementations of Visit and VisitMut that are not able to be automatically derived yet.
// Ideally everything would be derived.
use super::*;
use crate::visitors::{Visit, VisitMut, Visitor, VisitorMut};

// The following contain type signatures, which are addendums to previous identities
impl Visit for FunctionBody {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_function_body(self);
        self.parameters.start.visit(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement Visit for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        for parameter in &self.parameters.inner {
            parameter.visit(visitor);
            type_specifiers.next().visit(visitor);
        }

        self.parameters.end.visit(visitor);

        #[cfg(feature = "roblox")]
        self.return_type.visit(visitor);

        self.block.visit(visitor);
        self.end_token.visit(visitor);
        visitor.visit_function_body_end(self);
    }
}

impl VisitMut for FunctionBody {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_function_body(self);
        self.parameters.start = self.parameters.start.visit_mut(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers.into_iter();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement VisitMut for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        let mut new_type_specifiers = Vec::new();
        let mut new_parameters = Punctuated::new();

        for parameter_pair in self.parameters.inner.into_pairs() {
            let parameter_tuple = parameter_pair.into_tuple();
            let parameter = parameter_tuple.0.visit_mut(visitor);

            let type_specifier = type_specifiers
                .next()
                .and_then(|type_specifier| type_specifier)
                .map(|type_specifier| type_specifier.visit_mut(visitor));
            new_type_specifiers.push(type_specifier);
            let punctuation = parameter_tuple.1.visit_mut(visitor);

            new_parameters.push(Pair::new(parameter, punctuation));
        }

        self.parameters.inner = new_parameters;

        #[cfg(feature = "roblox")]
        {
            self.type_specifiers = new_type_specifiers;
        }

        self.parameters.end = self.parameters.end.visit_mut(visitor);

        #[cfg(feature = "roblox")]
        {
            self.return_type = self.return_type.visit_mut(visitor);
        }

        self.block = self.block.visit_mut(visitor);
        self.end_token = self.end_token.visit_mut(visitor);
        self = visitor.visit_function_body_end(self);
        self
    }
}

impl Visit for LocalAssignment {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_local_assignment(self);
        self.local_token.visit(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement Visit for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        for name in &self.name_list {
            name.visit(visitor);
            type_specifiers.next().visit(visitor);
        }

        self.equal_token.visit(visitor);
        self.expr_list.visit(visitor);
        visitor.visit_local_assignment_end(self);
    }
}

impl VisitMut for LocalAssignment {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_local_assignment(self);
        self.local_token = self.local_token.visit_mut(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers.into_iter();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement VisitMut for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        let mut new_type_specifiers = Vec::new();
        let mut new_names = Punctuated::new();

        for parameter_pair in self.name_list.into_pairs() {
            let parameter_tuple = parameter_pair.into_tuple();
            let parameter = parameter_tuple.0.visit_mut(visitor);
            let type_specifier = type_specifiers
                .next()
                .and_then(|type_specifier| type_specifier)
                .map(|type_specifier| type_specifier.visit_mut(visitor));

            let punctuation = parameter_tuple.1.visit_mut(visitor);
            new_type_specifiers.push(type_specifier);
            new_names.push(Pair::new(parameter, punctuation));
        }

        self.name_list = new_names;

        #[cfg(feature = "roblox")]
        {
            self.type_specifiers = new_type_specifiers;
        }

        self.equal_token = self.equal_token.visit_mut(visitor);
        self.expr_list = self.expr_list.visit_mut(visitor);
        self = visitor.visit_local_assignment_end(self);
        self
    }
}

impl Visit for GenericFor {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_generic_for(self);
        self.for_token.visit(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement Visit for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        for name in &self.names {
            name.visit(visitor);
            type_specifiers.next().visit(visitor);
        }

        self.in_token.visit(visitor);
        self.expr_list.visit(visitor);
        self.do_token.visit(visitor);
        self.block.visit(visitor);
        self.end_token.visit(visitor);

        visitor.visit_generic_for_end(self);
    }
}

impl VisitMut for GenericFor {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_generic_for(self);
        self.for_token = self.for_token.visit_mut(visitor);

        let mut type_specifiers;

        #[cfg(feature = "roblox")]
        {
            type_specifiers = self.type_specifiers.into_iter();
        }

        #[cfg(not(feature = "roblox"))]
        {
            // TODO: Option<!>, and implement VisitMut for !
            type_specifiers = std::iter::repeat::<Option<Self>>(None);
        }

        let mut new_type_specifiers = Vec::new();
        let mut new_names = Punctuated::new();

        for parameter_pair in self.names.into_pairs() {
            let parameter_tuple = parameter_pair.into_tuple();
            let parameter = parameter_tuple.0.visit_mut(visitor);
            let type_specifier = type_specifiers
                .next()
                .and_then(|type_specifier| type_specifier)
                .map(|type_specifier| type_specifier.visit_mut(visitor));

            let punctuation = parameter_tuple.1.visit_mut(visitor);
            new_type_specifiers.push(type_specifier);
            new_names.push(Pair::new(parameter, punctuation));
        }

        self.names = new_names;

        #[cfg(feature = "roblox")]
        {
            self.type_specifiers = new_type_specifiers;
        }

        self.in_token = self.in_token.visit_mut(visitor);
        self.expr_list = self.expr_list.visit_mut(visitor);
        self.do_token = self.do_token.visit_mut(visitor);
        self.block = self.block.visit_mut(visitor);
        self.end_token = self.end_token.visit_mut(visitor);

        self = visitor.visit_generic_for_end(self);
        self
    }
}

impl Visit for NumericFor {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_numeric_for(self);
        self.for_token.visit(visitor);
        self.index_variable.visit(visitor);

        #[cfg(feature = "roblox")]
        self.type_specifier.visit(visitor);

        self.equal_token.visit(visitor);
        self.start.visit(visitor);
        self.start_end_comma.visit(visitor);
        self.end.visit(visitor);
        self.end_step_comma.visit(visitor);
        self.step.visit(visitor);
        self.do_token.visit(visitor);
        self.block.visit(visitor);
        self.end_token.visit(visitor);

        visitor.visit_numeric_for_end(self);
    }
}

impl VisitMut for NumericFor {
    fn visit_mut<V: VisitorMut>(mut self, visitor: &mut V) -> Self {
        self = visitor.visit_numeric_for(self);
        self.for_token = self.for_token.visit_mut(visitor);
        self.index_variable = self.index_variable.visit_mut(visitor);

        #[cfg(feature = "roblox")]
        {
            self.type_specifier = self.type_specifier.visit_mut(visitor);
        }

        self.equal_token = self.equal_token.visit_mut(visitor);
        self.start = self.start.visit_mut(visitor);
        self.start_end_comma = self.start_end_comma.visit_mut(visitor);
        self.end = self.end.visit_mut(visitor);
        self.end_step_comma = self.end_step_comma.visit_mut(visitor);
        self.step = self.step.visit_mut(visitor);
        self.do_token = self.do_token.visit_mut(visitor);
        self.block = self.block.visit_mut(visitor);
        self.end_token = self.end_token.visit_mut(visitor);

        self = visitor.visit_numeric_for_end(self);
        self
    }
}
