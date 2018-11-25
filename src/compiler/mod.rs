use ast::*;
use object::*;
use bytecode::*;
use std::fmt;

#[derive(Copy, Debug)]
struct Bytecode {
    instructions: Instructions,
    constants: Vec<Object>,
}

struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}
#[derive(Debug)]
struct CompilerError {
    message: String,
}

type CompilerResult<T> = Result<T, CompilerError>;

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
        }
    }

    pub fn compile<T>(&mut self) -> CompilerResult<T> {
        Err(CompilerError {
            message: "Compiler not implemented yet.".to_string()
        })
    }

    pub fn bytecode(&mut self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.copy(),
            constants: self.constants.copy(),
        }
    }
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CompilerError {
    fn description(&self) -> &str {
        &self.message
    }
}

