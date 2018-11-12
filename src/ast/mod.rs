use token;
use std::fmt;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub enum Node {
    Program(Box<Program>),
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Statement {
    Let(Box<LetStatement>),
    Return(Box<ReturnStatement>),
    Expression(Box<ExpressionStatement>),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Statement::Let(statement) => format!("{}", statement),
            Statement::Return(ret) => format!("{}", ret),
            Statement::Expression(expression) => format!("{}", expression),
        };
        
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Expression {
    Identifier(String),
    Integer(i64),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    Boolean(bool),
    Str(String),
    If(Box<IfExpression>),
    Function(Box<FunctionLiteral>),
    Call(Box<CallExpression>),
    Array(Box<ArrayLiteral>),
    Index(Box<IndexExpression>),
    Hash(Box<HashLiteral>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Expression::Identifier(s) => s.clone(),
            Expression::Integer(value) => format!("{}", value),
            Expression::Prefix(prefix) => prefix.to_string(),
            Expression::Infix(infix) => infix.to_string(),
            Expression::Boolean(b) => b.to_string(),
            Expression::Str(s) => s.clone(),
            Expression::If(expression) => expression.to_string(),
            Expression::Function(func) => func.to_string(),
            Expression::Call(call) => call.to_string(),
            Expression::Array(array) => array.to_string(),
            Expression::Index(index) => index.to_string(),
            Expression::Hash(hash) => hash.to_string(),
        };

        write!(f, "{}", s)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct HashLiteral {
    pub pairs: HashMap<Expression, Expression>,
}

impl Hash for HashLiteral {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash trait not implemented for HashLiteral");
    }
}

impl fmt::Display for HashLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pairs: Vec<String> = (&self.pairs).into_iter().map(|(k, v)| format!("{}:{}", k.to_string(), v.to_string())).collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct IfExpression {
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;

        if let Some(ref statement) = self.alternative {
            write!(f, "else {}", statement)?;
        }
        Ok(())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct ArrayLiteral {
    pub elements: Vec<Expression>,
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let elements: Vec<String> = (&self.elements).into_iter().map(|e| e.to_string()).collect();
        write!(f, "[{}]", elements.join(", "))
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct IndexExpression {
    pub left: Expression,
    pub index: Expression,
}

impl fmt::Display for IndexExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}[{}])", self.left.to_string(), self.index.to_string())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct FunctionLiteral {
    pub parameters: Vec<IdentifierExpression>,
    pub body: BlockStatement,
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let param_list: Vec<String> = (&self.parameters).into_iter().map(|p| p.to_string()).collect();
        write!(f, "({}) {}", param_list.join(", "), self.body)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct CallExpression {
    pub function: Expression,
    pub arguments: Vec<Expression>,
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let arg_list: Vec<String> = (&self.arguments).into_iter().map(|exp| exp.to_string()).collect();
        write!(f, "{}({})", self.function.to_string(), arg_list.join(", "))
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct IdentifierExpression {
    pub name: String,
}

impl fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct PrefixExpression {
    pub operator: token::Token,
    pub right: Expression,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct InfixExpression {
    pub operator: token::Token,
    pub left: Expression,
    pub right: Expression,
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct IntegerLiteral {
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct LetStatement {
    pub name: String,
    pub value: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program { statements: Vec::new(), }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let statements: Vec<String> = (&self.statements).into_iter().map(|s| s.to_string()).collect();
        write!(f, "{}", statements.join(""))
    }
}