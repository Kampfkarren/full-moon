#[doc(hidden)]
#[macro_export]
macro_rules! make_op {
    ($enum:ident, $(#[$outer:meta])* { $($(#[$inner:meta])* $operator:ident,)+ }) => {
        #[derive(Clone, Debug, Display, PartialEq, Eq, Node, Visit)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        #[non_exhaustive]
        $(#[$outer])*
        #[display(fmt = "{}")]
        pub enum $enum {
            $(
                $(#[$inner])*
                #[allow(missing_docs)]
                $operator(TokenReference),
            )+
        }
    };
}

// Consumes a ParserResult into an Option<T>.
// If the ParserResult is LexerMoved, this signifies an unrecoverable error, and
// the function exits early.
// I wish we had Try traits.
#[doc(hidden)]
#[macro_export]
macro_rules! try_parser {
    ($result:expr) => {
        match $result {
            $crate::ast::parser_structs::ParserResult::Value(value) => Some(value),
            $crate::ast::parser_structs::ParserResult::NotFound => None,
            $crate::ast::parser_structs::ParserResult::LexerMoved => {
                return $crate::ast::parser_structs::ParserResult::LexerMoved
            }
        }
    };
}
