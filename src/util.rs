use crate::tokenizer::TokenReference;
use std::{
    borrow::{Borrow, Cow},
    fmt::Display,
};

pub fn display_option<T: Display, O: Borrow<Option<T>>>(option: O) -> String {
    match option.borrow() {
        Some(x) => x.to_string(),
        None => "".to_string(),
    }
}

pub fn display_optional_punctuated<T: Display>(
    pair: &(T, Option<Cow<'_, TokenReference<'_>>>),
) -> String {
    format!("{}{}", pair.0, display_option(&pair.1))
}

pub fn display_optional_punctuated_vec<T: Display>(
    vec: &[(T, Option<Cow<'_, TokenReference<'_>>>)],
) -> String {
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

#[cfg(feature = "roblox")]
pub fn join_type_specifiers<'a, I: IntoIterator<Item = Option<T2>>, T1: Display, T2: Display>(
    parameters: &Punctuated<'a, T1>,
    type_specifiers: I,
) -> String {
    let mut string = String::new();

    for (parameter, type_specifier) in parameters.pairs().zip(
        type_specifiers
            .into_iter()
            .chain(std::iter::repeat_with(|| None)),
    ) {
        string.push_str(&format!(
            "{}{}{}",
            parameter.value(),
            display_option(type_specifier),
            display_option(parameter.punctuation())
        ));
    }

    string
}
