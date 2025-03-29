#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetStmt {
        name: String,
        value: Expression,
    },
    ReturnStmt(Expression),
    ExpressionStmt(Expression),
    FunctionDecl {
        name: String,
        parameters: Vec<String>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Unary {
        operator: String,
        right: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: Vec<Statement>,
        alternative: Option<Vec<Statement>>,
    },
    Function {
        parameters: Vec<String>,
        body: Vec<Statement>,
    },
    Call {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    StrLit(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub repr: String,
}
