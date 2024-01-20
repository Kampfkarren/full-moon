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

#[doc(hidden)]
#[macro_export]
macro_rules! version_switch {
    ($lua_version:expr, {
        $(
            $($version:ident)|* => $value:expr
        )+
    }) => {
        paste::paste! {
            $(
                $(
                    #[cfg(feature = "" $version)]
                    if $lua_version.[<has_ $version>]() {
                        $value
                    }
                )*
            )+
        }
    };
}
