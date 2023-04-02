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
