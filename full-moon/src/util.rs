use crate::tokenizer::TokenReference;
use std::{borrow::Borrow, fmt::Display};

use crate::ast::punctuated::Punctuated;
use std::fmt::Write;

// Check if the vector is empty or full of None's
#[cfg(any(feature = "lua54", feature = "luau"))]
#[allow(clippy::ptr_arg)]
pub fn empty_optional_vector<T>(vec: &Vec<Option<T>>) -> bool {
    vec.iter().all(Option::is_none)
}

// This allows to check if the vector is full of None's -> true
// If the vector length is 0 or there is at least one Some -> false
#[cfg(any(feature = "luau"))]
#[allow(clippy::ptr_arg)]
pub fn no_luau_usage<T>(vec: &Vec<Option<T>>) -> bool {
    vec.iter().all(Option::is_none) && !vec.is_empty()
}

pub fn display_option<T: Display, O: Borrow<Option<T>>>(option: O) -> String {
    match option.borrow() {
        Some(x) => x.to_string(),
        None => "".to_string(),
    }
}

pub fn display_optional_punctuated<T: Display>(pair: &(T, Option<TokenReference>)) -> String {
    format!("{}{}", pair.0, display_option(&pair.1))
}

pub fn display_optional_punctuated_vec<T: Display>(vec: &[(T, Option<TokenReference>)]) -> String {
    let mut string = String::new();

    for pair in vec {
        string.push_str(&display_optional_punctuated(pair));
    }

    string
}

pub fn join_vec<T: Display, V: AsRef<[T]>>(vec: V) -> String {
    let mut string = String::new();

    for item in vec.as_ref() {
        string.push_str(&item.to_string());
    }

    string
}

#[cfg(feature = "luau")]
pub fn join_type_specifiers<I: IntoIterator<Item = Option<T2>>, T1: Display, T2: Display>(
    parameters: &Punctuated<T1>,
    type_specifiers: I,
) -> String {
    let mut string = String::new();

    for (parameter, type_specifier) in parameters.pairs().zip(
        type_specifiers
            .into_iter()
            .chain(std::iter::repeat_with(|| None)),
    ) {
        let _ = write!(
            string,
            "{}{}{}",
            parameter.value(),
            display_option(type_specifier),
            display_option(parameter.punctuation())
        );
    }

    string
}

pub fn join_iterators<
    I1: IntoIterator<Item = Option<T2>>,
    I2: IntoIterator<Item = Option<T3>>,
    T1: Display,
    T2: Display,
    T3: Display,
>(
    parameters: &Punctuated<T1>,
    first_iterator: I1,
    second_iterator: I2,
) -> String {
    let mut string = String::new();

    for ((name, item1), item2) in parameters
        .pairs()
        .zip(first_iterator.into_iter())
        .zip(second_iterator.into_iter())
    {
        let _ = write!(
            string,
            "{}{}{}{}",
            name.value(),
            display_option(item1),
            display_option(item2),
            display_option(name.punctuation())
        );
    }

    string
}

#[doc(hidden)]
#[macro_export]
macro_rules! has_version {
    ($lua_state:expr, ) => {
        true
    };

    ($lua_version:expr, $($version:ident,)+) => {{
        paste::paste! {
            let mut version_passes = false;

            $(
                #[cfg(feature = "" $version)]
                if $lua_version.[<has_ $version>]() {
                    version_passes = true;
                }
            )+

            version_passes
        }}
    };
}
