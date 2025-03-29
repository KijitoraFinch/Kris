use crate::ast::*;

#[derive(Debug, Clone)]
pub struct Environment {
    pub variables: std::collections::HashMap<String, Literal>,
}

#[derive(Debug, Clone)]
pub struct Evaluator {
    pub env: Environment,
}
