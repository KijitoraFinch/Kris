use crate::ast::*;

#[derive(Debug, Clone)]
pub struct Environment {
    pub variables: std::collections::HashMap<String, Literal>,
}

#[derive(Debug, Clone)]
pub struct Evaluator {
    pub env: Environment,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            env: Environment {
                variables: std::collections::HashMap::new(),
            },
        }
    }

    pub fn eval(&mut self, program: &Program) -> Result<Literal, String> {
        for statement in &program.statements {
            match statement {
                Statement::LetStmt { name, value } => {
                    let evaluated_value = self.eval_expression(value)?;
                    self.env.variables.insert(name.clone(), evaluated_value);
                }
                Statement::ReturnStmt(value) => {
                    return self.eval_expression(value);
                }
                Statement::ExpressionStmt(value) => {
                    self.eval_expression(value)?;
                }
                _ => {
                    todo!()
                }
            }
        }
        Err("No return value".to_string())
    }

    pub fn eval_expression(&mut self, expr: &Expression) -> Result<Literal, String> {
        match expr {
            Expression::Ident(ident) => {
                if let Some(value) = self.env.variables.get(&ident.repr) {
                    Ok(value.clone())
                } else {
                    Err(format!("Undefined variable: {}", ident.repr))
                }
            }
            Expression::Literal(literal) => Ok(literal.clone()),
            Expression::Unary { operator, right } => {
                let right_value = self.eval_expression(right)?;
                match operator.as_str() {
                    "-" => match right_value {
                        Literal::Int(i) => Ok(Literal::Int(-i)),
                        _ => Err("Invalid unary operation".to_string()),
                    },
                    "+" => match right_value {
                        Literal::Int(i) => Ok(Literal::Int(i)),
                        _ => Err("Invalid unary operation".to_string()),
                    },
                    "!" => match right_value {
                        Literal::Bool(b) => Ok(Literal::Bool(!b)),
                        _ => Err("Invalid unary operation".to_string()),
                    },
                    _ => Err("Unknown operator".to_string()),
                }
            }
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left_value = self.eval_expression(left)?;
                let right_value = self.eval_expression(right)?;
                match operator.as_str() {
                    "+" => match (left_value, right_value) {
                        (Literal::Int(l), Literal::Int(r)) => Ok(Literal::Int(l + r)),
                        _ => Err("Invalid binary operation".to_string()),
                    },
                    "-" => match (left_value, right_value) {
                        (Literal::Int(l), Literal::Int(r)) => Ok(Literal::Int(l - r)),
                        _ => Err("Invalid binary operation".to_string()),
                    },
                    "*" => match (left_value, right_value) {
                        (Literal::Int(l), Literal::Int(r)) => Ok(Literal::Int(l * r)),
                        _ => Err("Invalid binary operation".to_string()),
                    },
                    "/" => match (left_value, right_value) {
                        (Literal::Int(l), Literal::Int(r)) if r != 0 => Ok(Literal::Int(l / r)),
                        _ => Err("Invalid binary operation".to_string()),
                    },
                    _ => Err("Unknown operator".to_string()),
                }
            }
            _ => Err("Unsupported expression type".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        let mut evaluator = Evaluator::new();
        let program = Program {
            statements: vec![
                Statement::LetStmt {
                    name: "x".to_string(),
                    value: Expression::Literal(Literal::Int(5)),
                },
                Statement::LetStmt {
                    name: "y".to_string(),
                    value: Expression::Literal(Literal::Int(10)),
                },
                Statement::ReturnStmt(Expression::Binary {
                    left: Box::new(Expression::Ident(Ident {
                        repr: "x".to_string(),
                    })),
                    operator: "+".to_string(),
                    right: Box::new(Expression::Ident(Ident {
                        repr: "y".to_string(),
                    })),
                }),
            ],
        };

        let result = evaluator.eval(&program);
        assert_eq!(result, Ok(Literal::Int(15)));
    }
}
