use logos::Lexer;

use crate::atom::{Atom, InterpolatedStringBegin, InterpolatedStringSection};

fn read_interpolated_string_section<T>(
    lexer: &mut Lexer<Atom>,
    format_type: T,
    end_type: T,
) -> Option<T> {
    let mut escape = false;
    let mut characters = lexer.remainder().chars().peekable();

    while let Some(character) = characters.next() {
        match (escape, character) {
            (true, ..) => escape = false,

            // Make sure \u{1234} doesn't get treated as \u, followed by {1234}
            (false, '\\') if characters.peek() == Some(&'u') => {
                characters.next();
                lexer.bump(1);

                if characters.peek() == Some(&'{') {
                    characters.next();
                    lexer.bump(1);

                    for character in characters.by_ref() {
                        lexer.bump(character.len_utf8());

                        if character == '}' {
                            break;
                        }
                    }
                }
            }

            (false, '\\') => escape = true,
            (false, '\n' | '\r') => break,

            (false, ..) if character == '{' => {
                lexer.bump(1);
                lexer.extras.brace_stack.push(BraceType::InterpolatedString);
                return Some(format_type);
            }

            (false, ..) if character == '`' => {
                lexer.bump(1);
                return Some(end_type);
            }

            _ => {}
        }

        lexer.bump(character.len_utf8());
    }

    None
}

pub(crate) fn read_interpolated_string_begin(
    lexer: &mut Lexer<Atom>,
) -> Option<InterpolatedStringBegin> {
    read_interpolated_string_section(
        lexer,
        InterpolatedStringBegin::Formatted,
        InterpolatedStringBegin::Simple,
    )
}

// Some(None) = }
// Some(Some(section)) = interpolated string section
// None = no match
pub(crate) fn read_interpolated_string_right_brace(
    lexer: &mut Lexer<Atom>,
) -> Option<Option<InterpolatedStringSection>> {
    if lexer.extras.brace_stack.pop() != Some(BraceType::InterpolatedString) {
        return Some(None); // Just a normal brace
    }

    Some(Some(read_interpolated_string_section(
        lexer,
        InterpolatedStringSection::Middle,
        InterpolatedStringSection::End,
    )?)) // ? needed so that it does not turn into Some(None), which is an }, which this is not
}

pub(crate) fn read_left_brace(lexer: &mut Lexer<Atom>) -> bool {
    lexer.extras.brace_stack.push(BraceType::Normal);
    true
}
