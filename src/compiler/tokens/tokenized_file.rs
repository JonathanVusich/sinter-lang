use crate::compiler::tokens::token::Token;

#[derive(Debug, Default)]
pub struct TokenizedInput {
    tokens: Vec<Token>,
    line_map: Vec<usize>,
    line_counter: usize,
}

#[derive(Eq, PartialEq, Debug)]
pub struct TokenPosition {
    pub line: usize,
    pub pos: usize,
}

impl TokenizedInput {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line_map: Vec::new(),
            line_counter: 0,
        }
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn token_position(&self, pos: usize) -> TokenPosition {
        let line = self.line_map.partition_point(|&line_pos| line_pos <= pos);

        let line_pos = if line > 0 {
            pos - self.line_map[line - 1]
        } else {
            pos
        };
        TokenPosition::new(line, line_pos)
    }

    pub fn add_line_break(&mut self, pos: usize) {
        self.line_map.push(pos);
        self.line_counter += 1;
    }

    pub fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }
}

impl TokenPosition {
    pub fn new(line: usize, pos: usize) -> Self {
        Self { line, pos }
    }
}

mod tests {
    use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};

    #[test]
    pub fn line_map() {
        let mut tokenized_input = TokenizedInput::new();

        tokenized_input.add_line_break(2);
        tokenized_input.add_line_break(5);

        assert_eq!(TokenPosition::new(0, 0), tokenized_input.token_position(0));
        assert_eq!(TokenPosition::new(0, 1), tokenized_input.token_position(1));
        assert_eq!(TokenPosition::new(1, 0), tokenized_input.token_position(2));
        assert_eq!(TokenPosition::new(1, 1), tokenized_input.token_position(3));
        assert_eq!(TokenPosition::new(1, 2), tokenized_input.token_position(4));
        assert_eq!(TokenPosition::new(2, 0), tokenized_input.token_position(5));
    }
}
