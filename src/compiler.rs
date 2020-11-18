use crate::scanner::Scanner;
use crate::chunk::{Chunk, ConstantIndex};
use crate::token::{Token, TokenType};
use crate::token::TokenType::*;
use std::str::FromStr;
use crate::opcode::{OP_F64_CONSTANT, OP_I64_CONSTANT, OP_F64_CONSTANT_2_BYTES, OP_I64_CONSTANT_2_BYTES};

pub (crate) struct Compiler<'a> {
    scanner: Scanner<'a>,
    previous_token: Token,
    current_token: Token,
    chunk: Chunk,
    had_error: bool,
    panic_mode: bool
}

impl Compiler<'_> {

    pub (crate) fn new(code: &str) -> Compiler {
        return Compiler {
            scanner: Scanner::new(code),
            previous_token: Token::from_str(
                TokenError,
                "",
                0
            ),
            current_token: Token::from_str(
                TokenError,
                "",
                0
            ),
            chunk: Chunk::new(),
            had_error: false,
            panic_mode: false
        }
    }

    pub (crate) fn compile(&mut self) -> Option<&Chunk> {
        self.advance();
        self.expression();
        self.consume(TokenEof, &"Expect end of expression.");
        return if self.had_error {
            Option::None
        } else {
            Option::Some(&self.chunk)
        }
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.previous_token, &mut self.current_token);
        loop {
            self.current_token = self.scanner.scan_token();
            if self.current_token.token_type != TokenError {
                break;
            }

            let error_message = self.current_token.token.clone();
            self.error_at_current(&error_message);
        }
    }

    fn expression(&mut self) {

    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenRightParen, &"Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_type = self.previous_token.token_type;

        self.expression();

        match operator_type {
            TokenMinus => {
                self.chunk.add_byte()
            }
            _ => {}
        }
    }

    fn number(&mut self) {
        let token_type = &self.previous_token.token_type;
        let str = &self.previous_token.token;
        let line = self.previous_token.line;

        match token_type {
            TokenLong => {
                let val: i64 = i64::from_str(str).unwrap();

                // Add to chunk
                let index = self.chunk.add_i64_constant(val);
                match index {
                    ConstantIndex::OneByte(val) => {
                        self.chunk.add_byte(OP_I64_CONSTANT, line);
                        self.chunk.add_byte(val, line);
                    },
                    ConstantIndex::TwoBytes(val) => {
                        self.chunk.add_byte(OP_I64_CONSTANT_2_BYTES, line);
                        self.chunk.add_short(val, line);
                    }
                    ConstantIndex::None => {
                        self.error(&"Cannot have more than 65536 long constants.")
                    }
                }
            },
            TokenDouble => {
                let val: f64 = f64::from_str(str).unwrap();

                // Add to chunk
                let index = self.chunk.add_f64_constant(val);
                match index {
                    ConstantIndex::OneByte(val) => {
                        self.chunk.add_byte(OP_F64_CONSTANT, line);
                        self.chunk.add_byte(val, line);
                    },
                    ConstantIndex::TwoBytes(val) => {
                        self.chunk.add_byte(OP_F64_CONSTANT_2_BYTES, line);
                        self.chunk.add_short(val, line);
                    }
                    ConstantIndex::None => {
                        self.error(&"Cannot have more than 65536 double constants.")
                    }
                }
            },
            _ => {
                self.error(&"Expected valid numeric value.")
            }
        }
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk.add_byte(byte, self.previous_token.line);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn consume<T: AsRef<str>>(&mut self, token_type: TokenType, message: &T) {
        if self.current_token.token_type == token_type {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn error<T: AsRef<str>>(&mut self, message: &T) {
        let previous_token = self.previous_token.clone();
        self.error_at(&previous_token, message);
    }

    fn error_at_current<T: AsRef<str>>(&mut self, message: &T) {
        let current_token = self.current_token.clone();
        self.error_at(&current_token, message);
    }

    fn error_at<T: AsRef<str>>(&mut self, token: &Token, message: &T) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        if token.token_type == TokenEof {
            eprint!(" at end.");
        } else if token.token_type != TokenError {
            eprint!(" at {}", token.token);
        }

        eprintln!(": {}", message.as_ref());
        self.had_error = true;
    }


}

