use crate::ast::{self, *};
use crate::ast::{Expression, Ident, Literal, Statement};
use crate::lexer::Lexer;
use crate::prattutil::{OperatorLookup, OperatorMetadata, OperatorTable};
use crate::token::Token;

pub trait LexerLike {
    fn next_token(&mut self) -> Token;
}

#[derive(Debug)]
pub struct Parser<L: LexerLike> {
    lexer: L,
    cur_token: Token,
    peek_token: Token,
    operator_table: OperatorTable,
}

impl<L: LexerLike> Parser<L> {
    pub fn new(mut lexer: L) -> Self {
        let mut p = Parser {
            lexer,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            operator_table: OperatorTable::new(),
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn consume(&mut self, tok: Token) {
        if self.cur_token == tok {
            self.next_token();
        } else {
            panic!("expected token: {:?}, got: {:?}", tok, self.cur_token);
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            program.statements.push(stmt);
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Statement {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_with_bp(0)
    }

    fn parse_expression_with_bp(&mut self, bp: u8) -> Expression {
        let op_meta = self.operator_table.get_prefix(&self.cur_token);
        //TODO: refactor this shit
        let epilogue = match op_meta.clone() {
            Some(op) => op.epilogue,
            _ => None,
        };
        let mut lhs = match op_meta {
            Some(op) => {
                self.next_token();
                let expr = self.parse_expression_with_bp(op.rbp);
                Expression::Prefix {
                    operator: self.cur_token.to_string(),
                    right: Box::new(expr),
                }
            }
            None => match self.cur_token {
                // if no operator is found, we parse the token as an atomic expression
                Token::Symbol(_) => self.parse_identifier(),
                Token::Integer(_) => self.parse_integer_literal(),
                Token::StrLit(_) => {
                    let s = self.cur_token.to_string();
                    self.next_token();
                    Expression::Literal(Literal::StrLit(s))
                }
                _ => {
                    panic!("unexpected token: {:?}", self.cur_token);
                }
            },
        };

        // parse and combine following expressions
        while let Some(op) = self.operator_table.get_infix(&self.peek_token) {
            if op.lbp <= bp {
                break;
            }
            self.next_token();
            lhs = match lhs {
                Expression::Ident(_) | Expression::Literal(_) => {
                    let left = Box::new(lhs);
                    let right = Box::new(self.parse_expression_with_bp(op.rbp));
                    Expression::Infix {
                        left,
                        operator: self.cur_token.to_string(),
                        right,
                    }
                }
                _ => panic!("unexpected expression: {:?}", lhs),
            };
        }

        // if the current token is an epilogue, consume it
        if let Some(e) = epilogue {
            self.consume(e);
        }
        lhs
    }

    fn parse_let_statement(&mut self) -> Statement {
        self.consume(Token::Let);
        let name = match self.cur_token {
            Token::Symbol(ref s) => s.clone(),
            _ => panic!("expected identifier, got: {:?}", self.cur_token),
        };
        self.next_token();
        self.consume(Token::Assign);
        let value = self.parse_expression();
        self.consume(Token::Semicolon);
        Statement::LetStmt { name, value }
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.consume(Token::Return);
        let value = self.parse_expression();
        let stmt = Statement::ReturnStmt(value);
        self.consume(Token::Semicolon);
        stmt
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expr = match self.cur_token {
            Token::Symbol(_) | Token::Integer(_) | Token::StrLit(_) => self.parse_expression(),
            _ => panic!(
                "unexpected token in expression statement: {:?}",
                self.cur_token
            ),
        };
        let stmt = Statement::ExpressionStmt(expr);
        self.consume(Token::Semicolon);
        stmt
    }

    fn parse_identifier(&mut self) -> Expression {
        let expr = match self.cur_token {
            Token::Symbol(ref s) => {
                let ident = Ident { repr: s.clone() };
                Expression::Ident(ident)
            }
            _ => panic!("expected identifier, got: {:?}", self.cur_token),
        };
        self.next_token();
        expr
    }

    fn parse_integer_literal(&mut self) -> Expression {
        let expr = match self.cur_token {
            Token::Integer(i) => Expression::Literal(Literal::Int(i)),
            _ => panic!("expected integer literal, got: {:?}", self.cur_token),
        };
        self.next_token();
        expr
    }
}

impl LexerLike for crate::lexer::Lexer<'_> {
    fn next_token(&mut self) -> Token {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::LetStmt { name, value } => {
                assert_eq!(name, "x");
                match value {
                    Expression::Literal(Literal::Int(val)) => assert_eq!(*val, 5),
                    _ => panic!("Expected integer literal"),
                }
            }
            _ => panic!("Expected let statement"),
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "return 10;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ReturnStmt(expr) => match expr {
                Expression::Literal(Literal::Int(val)) => assert_eq!(*val, 10),
                _ => panic!("Expected integer literal"),
            },
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_expression_statements() {
        let input = "foobar;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStmt(expr) => match expr {
                Expression::Ident(ident) => assert_eq!(ident.repr, "foobar"),
                _ => panic!("Expected identifier"),
            },
            _ => panic!("Expected expression statement"),
        }
    }
}
