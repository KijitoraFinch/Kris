use crate::token::{self, Token};

pub trait OperatorLookup {
    fn get_prefix(&self, tok: &Token) -> Option<OperatorMetadata>;
    fn get_infix(&self, tok: &Token) -> Option<OperatorMetadata>;
    fn get_postfix(&self, tok: &Token) -> Option<OperatorMetadata>;
    fn register(&mut self, tok: Token, op: OperatorMetadata);
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperatorMetadata {
    pub lbp: u8,
    pub rbp: u8,
    pub epilogue: Option<Token>, // for paren-like operators. e.g. LParen -> RParen
}

#[derive(Debug)]
pub struct OperatorTable {}

impl OperatorTable {
    pub fn new() -> Self {
        OperatorTable {}
    }
}

impl OperatorLookup for OperatorTable {
    fn get_prefix(&self, tok: &Token) -> Option<OperatorMetadata> {
        match tok {
            Token::Minus => Some(OperatorMetadata {
                lbp: 0,
                rbp: 6,
                epilogue: None,
            }),
            Token::Bang => Some(OperatorMetadata {
                lbp: 0,
                rbp: 6,
                epilogue: None,
            }),
            Token::LParen => Some(OperatorMetadata {
                lbp: 0,
                rbp: 0,
                epilogue: Some(Token::RParen),
            }),
            _ => None,
        }
    }

    fn get_infix(&self, tok: &Token) -> Option<OperatorMetadata> {
        match tok {
            Token::Plus => Some(OperatorMetadata {
                lbp: 4,
                rbp: 3,
                epilogue: None,
            }),
            Token::Minus => Some(OperatorMetadata {
                lbp: 4,
                rbp: 3,
                epilogue: None,
            }),
            Token::Asterisk => Some(OperatorMetadata {
                lbp: 5,
                rbp: 6,
                epilogue: None,
            }),
            Token::Slash => Some(OperatorMetadata {
                lbp: 5,
                rbp: 6,
                epilogue: None,
            }),
            Token::Lt => Some(OperatorMetadata {
                lbp: 7,
                rbp: 7,
                epilogue: None,
            }),
            Token::Gt => Some(OperatorMetadata {
                lbp: 7,
                rbp: 7,
                epilogue: None,
            }),
            Token::Eq => Some(OperatorMetadata {
                lbp: 7,
                rbp: 7,
                epilogue: None,
            }),
            Token::NotEq => Some(OperatorMetadata {
                lbp: 7,
                rbp: 7,
                epilogue: None,
            }),
            Token::Pipe => Some(OperatorMetadata {
                lbp: 1,
                rbp: 2,
                epilogue: None,
            }),
            Token::Arrow => Some(OperatorMetadata {
                lbp: 1,
                rbp: 2,
                epilogue: None,
            }),
            Token::Assign => Some(OperatorMetadata {
                lbp: 1,
                rbp: 2,
                epilogue: None,
            }),
            _ => None,
        }
    }

    fn get_postfix(&self, tok: &Token) -> Option<OperatorMetadata> {
        match tok {
            Token::LParen => Some(OperatorMetadata {
                lbp: 0,
                rbp: 9,
                epilogue: Some(Token::RParen),
            }),
            _ => None,
        }
    }

    fn register(&mut self, tok: Token, op: OperatorMetadata) {
        unimplemented!()
    }
}
