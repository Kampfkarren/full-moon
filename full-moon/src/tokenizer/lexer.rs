use super::Token;

pub struct Lexer {
    source: LexerSource,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: LexerSource::new(source),
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        // todo
        None
    }
}

pub struct LexerSource {
    source: Vec<char>,
    offset: u64,
}

impl LexerSource {
    fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            offset: 0,
        }
    }

    fn current(&self) -> Option<char> {
        self.source.get(self.offset as usize).copied()
    }

    fn next(&mut self) -> Option<char> {
        self.offset += 1;
        self.current()
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.offset as usize + 1).copied()
    }
}
