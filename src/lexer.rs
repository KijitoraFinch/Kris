use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Lexer<'a> {
    input: &'a str,       // 入力文字列
    position: usize,      // 現在の文字の位置
    read_position: usize, // 次に読む文字の位置
    ch: char,             // 現在の文字
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.read_position).unwrap_or('\0')
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while self.ch.is_alphabetic() || self.ch == '_' {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        let mut has_dot = false;

        // 整数部分を読む
        while self.ch.is_digit(10) {
            self.read_char();
        }

        // 小数点があれば読む
        if self.ch == '.' && self.peek_char().is_digit(10) {
            has_dot = true;
            self.read_char(); // 小数点を消費

            // 小数部分を読む
            while self.ch.is_digit(10) {
                self.read_char();
            }
        }

        let number_str = self.input[position..self.position].to_string();

        if has_dot {
            // 浮動小数点数としてパース
            match number_str.parse::<f64>() {
                Ok(n) => Token::Float(n),
                Err(_) => Token::Illegal,
            }
        } else {
            // 整数としてパース
            match number_str.parse::<i64>() {
                Ok(n) => Token::Integer(n),
                Err(_) => Token::Illegal,
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::Eq
                }
                '>' => {
                    self.read_char();
                    self.read_char();
                    Token::Arrow
                }
                _ => {
                    self.read_char();
                    Token::Assign
                }
            },
            '+' => {
                self.read_char();
                Token::Plus
            }
            '-' => {
                self.read_char();
                Token::Minus
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    self.read_char();
                    Token::NotEq
                } else {
                    self.read_char();
                    Token::Bang
                }
            }
            '*' => {
                self.read_char();
                Token::Asterisk
            }
            '/' => {
                self.read_char();
                Token::Slash
            }
            '<' => {
                self.read_char();
                Token::Lt
            }
            '>' => {
                self.read_char();
                Token::Gt
            }
            '|' => match self.peek_char() {
                '>' => {
                    self.read_char();
                    self.read_char();
                    Token::Pipe
                }
                _ => {
                    self.read_char();
                    Token::Illegal
                }
            },
            '(' => {
                self.read_char();
                Token::LParen
            }
            ')' => {
                self.read_char();
                Token::RParen
            }
            '{' => {
                self.read_char();
                Token::LBrace
            }
            '}' => {
                self.read_char();
                Token::RBrace
            }
            ',' => {
                self.read_char();
                Token::Comma
            }
            ';' => {
                self.read_char();
                Token::Semicolon
            }
            ':' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    self.read_char();
                    Token::Assign
                }
                _ => {
                    self.read_char();
                    Token::Colon
                }
            },
            '@' => {
                self.read_char();
                Token::At
            }
            '"' => {
                self.read_char();
                Token::DoubleQuote
            }
            '|' => {
                self.read_char();
                Token::Bar
            }
            '\0' => Token::EOF,
            ch if ch.is_alphabetic() || ch == '_' => {
                let ident = self.read_identifier();
                return Token::lookup_keyword(&ident);
            }
            ch if ch.is_digit(10) => {
                return self.read_number();
            }
            _ => Token::Illegal,
        };

        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5; 
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            10 == 10;
            10 != 9;
            this_symbol_is_illegal_but_it_should_be_tokenized_as_symbol

            let block_value = {
                x + y
            };
            let anon_func = @(x, y) => x + y;
        "#;

        let expected = vec![
            Token::Let,
            Token::Symbol("five".to_string()),
            Token::Assign,
            Token::Integer(5),
            Token::Semicolon,
            Token::Let,
            Token::Symbol("ten".to_string()),
            Token::Assign,
            Token::Integer(10),
            Token::Semicolon,
            Token::Let,
            Token::Symbol("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Symbol("x".to_string()),
            Token::Comma,
            Token::Symbol("y".to_string()),
            Token::RParen,
            Token::LBrace,
            Token::Symbol("x".to_string()),
            Token::Plus,
            Token::Symbol("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Symbol("result".to_string()),
            Token::Assign,
            Token::Symbol("add".to_string()),
            Token::LParen,
            Token::Symbol("five".to_string()),
            Token::Comma,
            Token::Symbol("ten".to_string()),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Integer(5),
            Token::Semicolon,
            Token::Integer(5),
            Token::Lt,
            Token::Integer(10),
            Token::Gt,
            Token::Integer(5),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Integer(5),
            Token::Lt,
            Token::Integer(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Integer(10),
            Token::Eq,
            Token::Integer(10),
            Token::Semicolon,
            Token::Integer(10),
            Token::NotEq,
            Token::Integer(9),
            Token::Semicolon,
            Token::Symbol(
                "this_symbol_is_illegal_but_it_should_be_tokenized_as_symbol".to_string(),
            ),
            Token::Let,
            Token::Symbol("block_value".to_string()),
            Token::Assign,
            Token::LBrace,
            Token::Symbol("x".to_string()),
            Token::Plus,
            Token::Symbol("y".to_string()),
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Symbol("anon_func".to_string()),
            Token::Assign,
            Token::At,
            Token::LParen,
            Token::Symbol("x".to_string()),
            Token::Comma,
            Token::Symbol("y".to_string()),
            Token::RParen,
            Token::Arrow,
            Token::Symbol("x".to_string()),
            Token::Plus,
            Token::Symbol("y".to_string()),
            Token::Semicolon,
            Token::EOF,
        ];

        let mut lexer = Lexer::new(input);
        for expected_token in expected {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }
}
