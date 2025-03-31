use crate::ast::{self, *};
use crate::ast::{Expression, Ident, Literal, Statement};
use crate::lexer::Lexer;
use crate::prattutil::{OperatorLookup, OperatorMetadata, OperatorTable};
use crate::token::Token;
use std::collections::btree_map::Keys;
use std::fmt::Debug;

pub trait LexerLike: Debug + Clone {
    fn next_token(&mut self) -> Token;
}

#[derive(Debug)]
pub struct Parser<L: LexerLike> {
    lexer: L,
    cur_token: Token,
    peek_token: Token,
    operator_table: OperatorTable,
    level: u8,
}

impl<L: LexerLike> Parser<L> {
    pub fn new(mut lexer: L) -> Self {
        let mut p = Parser {
            lexer,
            cur_token: Token::EOF,
            peek_token: Token::EOF,
            operator_table: OperatorTable::new(),
            level: 0,
        };
        p.next_token();
        p.next_token();
        p
    }
}
impl<L: LexerLike> Parser<L> {
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
        }
        program
    }

    fn parse_statement(&mut self) -> Statement {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Function => self.parse_function_declaration(),
            _ => self.parse_expression_statement(),
        }
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
        let expr = self.parse_expression();
        self.consume(Token::Semicolon);
        Statement::ExpressionStmt(expr)
    }

    fn parse_function_declaration(&mut self) -> Statement {
        self.consume(Token::Function);
        let name = match self.cur_token {
            Token::Symbol(ref s) => s.clone(),
            _ => panic!("expected identifier, got: {:?}", self.cur_token),
        };
        self.next_token();
        self.consume(Token::LParen);
        let mut parameters = vec![];
        while self.cur_token != Token::RParen {
            match self.cur_token {
                Token::Symbol(ref s) => parameters.push(s.clone()),
                _ => panic!("expected identifier, got: {:?}", self.cur_token),
            }
            self.next_token();
            if self.cur_token == Token::Comma {
                self.next_token();
            }
        }
        self.consume(Token::RParen);
        let body = self.parse_code_block();
        Statement::FunctionDecl {
            name,
            parameters,
            body,
        }
    }

    fn parse_expression(&mut self) -> Expression {
        // block expression if the current token is '{'
        if self.cur_token == Token::LBrace {
            return self.parse_code_block();
        }
        self.parse_expression_with_bp(0)
    }

    fn parse_expression_with_bp(&mut self, bp: u8) -> Expression {
        if cfg!(debug_assertions) {
            dbg!(&self);
        }
        let op_meta = self.operator_table.get_prefix(&self.cur_token);
        //TODO: refactor this shit
        let epilogue = match op_meta.clone() {
            Some(op) => op.epilogue,
            _ => None,
        };
        let mut lhs = match op_meta {
            Some(op) => {
                let op_repr = self.cur_token.to_string();
                self.next_token();
                let expr = self.parse_expression_with_bp(op.rbp);
                if let Some(epilogue) = epilogue {
                    self.consume(epilogue);
                }
                if op_repr == "(" {
                    expr
                } else {
                    Expression::Unary {
                        operator: op_repr,
                        right: Box::new(expr),
                    }
                }
            }
            None => match self.cur_token {
                Token::Integer(_) => self.parse_integer_literal(),
                Token::Symbol(_) => self.parse_identifier(),
                Token::StrLit(_) => self.parse_string_literal(),
                Token::If => self.parse_if_expression(),
                _ => panic!("unexpected token: {:?}", self.cur_token),
            },
        };
        if cfg!(debug_assertions) {
            dbg!(&lhs);
            dbg!(&self.cur_token);
        }
        // parse and combine following expressions
        while let Some(op) = self.operator_table.get_infix(&self.cur_token) {
            if op.lbp <= bp {
                break;
            }
            let op_repr = self.cur_token.to_string();
            self.next_token();
            lhs = Expression::Binary {
                left: Box::new(lhs),
                operator: op_repr,
                right: Box::new(self.parse_expression_with_bp(op.rbp)),
            };
        }

        // if the current token is a postfix operator, consume it
        if let Some(op) = self.operator_table.get_postfix(&self.cur_token) {
            if op.lbp <= bp {
                let op_repr = self.cur_token.to_string();
                self.next_token();
                lhs = Expression::Unary {
                    operator: op_repr,
                    right: Box::new(lhs),
                };
            }
        }
        lhs
    }

    fn parse_if_expression(&mut self) -> Expression {
        self.consume(Token::If);
        let condition = self.parse_expression();
        let consequence = Box::new(self.parse_code_block());
        let alternative = if self.cur_token == Token::Else {
            self.next_token();
            Some(Box::new(self.parse_code_block()))
        } else {
            None
        };
        Expression::If {
            condition: Box::new(condition),
            consequence,
            alternative,
        }
    }

    fn parse_code_block(&mut self) -> Expression {
        self.consume(Token::LBrace);
        //dbg
        if cfg!(debug_assertions) {
            dbg!(&self.cur_token);
        }
        //dbg
        let mut statements = vec![];
        while self.cur_token != Token::RBrace && self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            statements.push(stmt);
        }
        self.consume(Token::RBrace);
        Expression::Block(statements)
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

    fn parse_string_literal(&mut self) -> Expression {
        let expr = match self.cur_token {
            Token::StrLit(ref s) => Expression::Literal(Literal::StrLit(s.clone())),
            _ => panic!("expected string literal, got: {:?}", self.cur_token),
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

    #[test]
    fn test_complicated_expression_1() {
        let input = "5 + 3 * 10;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        // With proper operator precedence, "5 + 3 * 10" should be parsed as "5 + (3 * 10)"
        // because "*" has higher precedence than "+"
        let expected_expr = Expression::Binary {
            left: Box::new(Expression::Literal(Literal::Int(5))),
            operator: "+".to_string(),
            right: Box::new(Expression::Binary {
                left: Box::new(Expression::Literal(Literal::Int(3))),
                operator: "*".to_string(),
                right: Box::new(Expression::Literal(Literal::Int(10))),
            }),
        };

        match &program.statements[0] {
            Statement::ExpressionStmt(expr) => {
                let expected_debug = format!("{:#?}", expected_expr);
                let actual_debug = format!("{:#?}", expr);

                if expected_debug != actual_debug {
                    println!("Expected AST:\n{}", expected_debug);
                    println!("Actual AST:\n{}", actual_debug);
                    panic!("AST does not match expected structure");
                }
            }
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_expression_with_paren() {
        let input = "(5 + 3) * 10;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        // With proper operator precedence, "(5 + 3) * 10" should be parsed as "((5 + 3) * 10)"
        // because "*" has higher precedence than "+"
        let expected_expr = Expression::Binary {
            left: Box::new(Expression::Binary {
                left: Box::new(Expression::Literal(Literal::Int(5))),
                operator: "+".to_string(),
                right: Box::new(Expression::Literal(Literal::Int(3))),
            }),
            operator: "*".to_string(),
            right: Box::new(Expression::Literal(Literal::Int(10))),
        };

        match &program.statements[0] {
            Statement::ExpressionStmt(expr) => {
                let expected_debug = format!("{:#?}", expected_expr);
                let actual_debug = format!("{:#?}", expr);

                if expected_debug != actual_debug {
                    println!("Expected AST:\n{}", expected_debug);
                    println!("Actual AST:\n{}", actual_debug);
                    panic!("AST does not match expected structure");
                }
            }
            _ => panic!("Expected expression statement"),
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { return x; } else { return y; };";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        if let Statement::ExpressionStmt(Expression::If {
            condition,
            consequence,
            alternative,
        }) = &program.statements[0]
        {
            // Test condition
            assert_eq!(
                **condition,
                Expression::Binary {
                    left: Box::new(Expression::Ident(Ident {
                        repr: "x".to_string()
                    })),
                    operator: "<".to_string(),
                    right: Box::new(Expression::Ident(Ident {
                        repr: "y".to_string()
                    })),
                }
            );

            // Test consequence
            if let Expression::Block(statements) = &**consequence {
                assert_eq!(statements.len(), 1);
                if let Statement::ReturnStmt(Expression::Ident(Ident { repr })) = &statements[0] {
                    assert_eq!(repr, "x");
                } else {
                    panic!("Expected return statement with identifier 'x'");
                }
            } else {
                panic!("Expected block expression for consequence");
            }

            // Test alternative
            if let Some(alt) = alternative {
                if let Expression::Block(statements) = &**alt {
                    assert_eq!(statements.len(), 1);
                    if let Statement::ReturnStmt(Expression::Ident(Ident { repr })) = &statements[0]
                    {
                        assert_eq!(repr, "y");
                    } else {
                        panic!("Expected return statement with identifier 'y'");
                    }
                } else {
                    panic!("Expected block expression for alternative");
                }
            } else {
                panic!("Expected Some alternative");
            }
        } else {
            panic!("Expected if expression");
        }
    }

    #[test]
    fn test_function_declaration() {
        let input = "fn add(x, y) { return x + y; }";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        if let Statement::FunctionDecl {
            name,
            parameters,
            body,
        } = &program.statements[0]
        {
            assert_eq!(name, "add");
            assert_eq!(parameters.len(), 2);
            assert_eq!(parameters[0], "x");
            assert_eq!(parameters[1], "y");

            // Check the function body
            if let Expression::Block(statements) = body {
                assert_eq!(statements.len(), 1);
                if let Statement::ReturnStmt(expr) = &statements[0] {
                    if let Expression::Binary {
                        left,
                        operator,
                        right,
                    } = expr
                    {
                        assert_eq!(operator, "+");
                        if let Expression::Ident(Ident { repr }) = left.as_ref() {
                            assert_eq!(repr, "x");
                        } else {
                            panic!("Expected identifier 'x'");
                        }
                        if let Expression::Ident(Ident { repr }) = right.as_ref() {
                            assert_eq!(repr, "y");
                        } else {
                            panic!("Expected identifier 'y'");
                        }
                    } else {
                        panic!("Expected binary expression x + y");
                    }
                } else {
                    panic!("Expected return statement");
                }
            } else {
                panic!("Expected block expression");
            }
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_code_block() {
        let input = "{ let x = 5; let y = 10; return x + y; };";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert_eq!(program.statements.len(), 1);

        match &program.statements[0] {
            Statement::ExpressionStmt(Expression::Block(statements)) => {
                assert_eq!(statements.len(), 3);
                if let Statement::LetStmt { name, value } = &statements[0] {
                    assert_eq!(name, "x");
                    if let Expression::Literal(Literal::Int(val)) = value {
                        assert_eq!(*val, 5);
                    } else {
                        panic!("Expected integer literal");
                    }
                } else {
                    panic!("Expected let statement");
                }

                if let Statement::LetStmt { name, value } = &statements[1] {
                    assert_eq!(name, "y");
                    if let Expression::Literal(Literal::Int(val)) = value {
                        assert_eq!(*val, 10);
                    } else {
                        panic!("Expected integer literal");
                    }
                } else {
                    panic!("Expected let statement");
                }

                if let Statement::ReturnStmt(expr) = &statements[2] {
                    if let Expression::Binary {
                        left,
                        operator,
                        right,
                    } = expr
                    {
                        assert_eq!(operator, "+");
                        if let Expression::Ident(Ident { repr }) = left.as_ref() {
                            assert_eq!(repr, "x");
                        } else {
                            panic!("Expected identifier 'x'");
                        }
                        if let Expression::Ident(Ident { repr }) = right.as_ref() {
                            assert_eq!(repr, "y");
                        } else {
                            panic!("Expected identifier 'y'");
                        }
                    } else {
                        panic!("Expected binary expression x + y");
                    }
                } else {
                    panic!("Expected return statement");
                }
            }
            _ => panic!("Expected block expression"),
        }
    }

    #[test]
    fn test_complex_expression() {
        let input = "let result = (5 + 3 * (10 - 2)) / (4 - 2) - -7;";
        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        // プログラムは1つのlet文を含むはず
        assert_eq!(program.statements.len(), 1);

        // let文のASTが期待通りになっているか検証
        match &program.statements[0] {
            Statement::LetStmt { name, value } => {
                assert_eq!(name, "result");

                // 期待するASTを構築
                let expected_expr = Expression::Binary {
                    left: Box::new(Expression::Binary {
                        left: Box::new(Expression::Binary {
                            left: Box::new(Expression::Literal(Literal::Int(5))),
                            operator: "+".to_string(),
                            right: Box::new(Expression::Binary {
                                left: Box::new(Expression::Literal(Literal::Int(3))),
                                operator: "*".to_string(),
                                right: Box::new(Expression::Binary {
                                    left: Box::new(Expression::Literal(Literal::Int(10))),
                                    operator: "-".to_string(),
                                    right: Box::new(Expression::Literal(Literal::Int(2))),
                                }),
                            }),
                        }),
                        operator: "/".to_string(),
                        right: Box::new(Expression::Binary {
                            left: Box::new(Expression::Literal(Literal::Int(4))),
                            operator: "-".to_string(),
                            right: Box::new(Expression::Literal(Literal::Int(2))),
                        }),
                    }),
                    operator: "-".to_string(),
                    right: Box::new(Expression::Unary {
                        operator: "-".to_string(),
                        right: Box::new(Expression::Literal(Literal::Int(7))),
                    }),
                };

                let expected_debug = format!("{:#?}", expected_expr);
                let actual_debug = format!("{:#?}", value);

                if expected_debug != actual_debug {
                    println!("Expected AST:\n{}", expected_debug);
                    println!("Actual AST:\n{}", actual_debug);
                    panic!("AST does not match expected structure");
                }
            }
            _ => panic!("Expected a let statement"),
        }
    }
}
