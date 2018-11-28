use ast;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Str(String),
    Return(Rc<Return>),
    Function(Rc<Function>),
    Builtin(Builtin),
    Array(Rc<Array>),
    Hash(Rc<MonkeyHash>),
    Null,
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Int(i) => i.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Str(s) => s.clone(),
            Object::Return(r) => r.value.inspect(),
            Object::Function(f) => f.inspect(),
            Object::Builtin(b) => b.inspect(),
            Object::Array(a) => a.inspect(),
            Object::Hash(h) => h.inspect(),
            Object::Null => String::from("null"),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.inspect())
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct MonkeyHash {
    pub pairs: HashMap<Rc<Object>, Rc<Object>>,
}

impl MonkeyHash {
    fn inspect(&self) -> String {
        let pairs: Vec<String> = (&self.pairs).into_iter().map(|(key, value)| format!("{}: {}", key.inspect(), value.inspect())).collect();
        format!("{{{}}}", pairs.join(", "))
    }
}

impl Hash for MonkeyHash {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash not implemented for monkey hash");
    }
}

#[derive(Clone, Debug)]
pub struct Array {
    pub elements: Vec<Rc<Object>>,
}

impl Array {
    fn inspect(&self) -> String {
        let elements: Vec<String> = (&self.elements).into_iter().map(|e| e.to_string()).collect();
        format!("[{}]", elements.join(", "))
    }
}

impl PartialEq for Array {
    fn eq(&self, other: &Array) -> bool {
        panic!("eq not implemented for array");
    }
}

impl Eq for Array {}

impl Hash for Array {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash for array not implemented");
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Puts,
}

impl Builtin {
    pub fn lookup(name: &str) -> Option<Object> {
        match name {
            "len" => Some(Object::Builtin(Builtin::Len)),
            "first" => Some(Object::Builtin(Builtin::First)),
            "last" => Some(Object::Builtin(Builtin::Last)),
            "rest" => Some(Object::Builtin(Builtin::Rest)),
            "push" => Some(Object::Builtin(Builtin::Push)),
            "puts" => Some(Object::Builtin(Builtin::Puts)),
            _ => None,
        }
    }

    pub fn apply(&self, args: &Vec<Rc<Object>>) -> Result<Rc<Object>, String> {
        match self {
            Builtin::Len => {
                if args.len() != 1 {
                    return Err("len takes only 1 array or string argument".to_string());
                }

                let arg = &*Rc::clone(args.first().unwrap());
                match arg {
                    Object::Str(s) => Ok(Rc::new(Object::Int(s.len() as i64))),
                    Object::Array(a) => Ok(Rc::new(Object::Int(a.elements.len() as i64))),
                    obj => Err(format!("object {:?} not supported as an argument for len", obj))
                }
            },
            Builtin::First => {
                if args.len() != 1 {
                    return Err("first takes only 1 array argument".to_string());
                }

                let arg = &*Rc::clone(args.first().unwrap());
                match arg {
                    Object::Array(a) => {
                        match a.elements.first() {
                            Some(el) => Ok(Rc::clone(el)),
                            None => Ok(Rc::new(Object::Null)),
                        }
                    },
                    obj => Err(format!("object {:?} not supported as an argument for first", obj))
                }
            },
            Builtin::Last => {
                if args.len() != 1 {
                    return Err("last takes only 1 array argument".to_string());
                }

                let arg = &*Rc::clone(args.first().unwrap());
                match arg {
                    Object::Array(a) => {
                        match a.elements.last() {
                            Some(el) => Ok(Rc::clone(el)),
                            None => Ok(Rc::new(Object::Null)),
                        }
                    },
                    obj => Err(format!("object {:?} not supported as an argument for last", obj))
                }
            },
            Builtin::Rest => {
                if args.len() != 1 {
                    return Err("rest takes only 1 array argument".to_string());
                }

                let arg = &*Rc::clone(args.first().unwrap());
                match arg {
                    Object::Array(a) => {
                        if a.elements.len() <= 1 {
                            Ok(Rc::new(Object::Array(Rc::new(Array{ elements: vec![] }))))
                        } else {
                            let mut elements = a.elements.clone();
                            elements.remove(0);
                            Ok(Rc::new(Object::Array(Rc::new(Array{ elements }))))
                        }
                    },
                    obj => Err(format!("object {:?} is not supported as an argument for rest", obj))
                }
            },
            Builtin::Push => {
                if args.len() != 2 {
                    return Err("push takes an array and an object".to_string());
                }

                let array = &*Rc::clone(args.first().unwrap());
                let obj = Rc::clone(args.last().unwrap());

                match array {
                    Object::Array(a) => {
                        let mut elements = a.elements.clone();
                        elements.push(obj);
                        Ok(Rc::new(Object::Array(Rc::new(Array{ elements }))))
                    },
                    _ => Err("first argument to push must be an array".to_string())
                }
            },
            Builtin::Puts => {
                for arg in args {
                    println!("{}", arg.inspect());
                }
                Ok(Rc::new(Object::Null))
            }
        }
    }

    fn inspect(&self) -> String {
        match self {
            Builtin::Len => "len".to_string(),
            Builtin::First => "first".to_string(),
            Builtin::Last => "last".to_string(),
            Builtin::Rest => "rest".to_string(),
            Builtin::Push => "push".to_string(),
            Builtin::Puts => "puts".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub parameters: Vec<ast::IdentifierExpression>,
    pub body: ast::BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Function {
    fn inspect(&self) -> String {
        let params: Vec<String> = (&self.parameters).into_iter().map(|p| p.to_string()).collect();
        format!("fn({}) {{\n{}\n}}", params.join(", "), self.body.to_string())
    }
}

impl PartialEq for Function {
    fn eq(&self, _other: &Function) -> bool {
        panic!("PartialEq not implemented for function");
    }
}

impl Eq for Function {}

impl Hash for Function {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("Hash for function is not supported");
    }
}

#[derive(Clone, Debug)]
pub struct Return {
    pub value: Rc<Object>,
}

impl PartialEq for Return {
    fn eq(&self, _other: &Return) -> bool {
        panic!("PartialEq not implemented for Return");
    }
}

impl Eq for Return {}

impl Hash for Return {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        // can't hash an array
        panic!("Hash for return not supported");
    }
}

#[derive(Clone, Debug)]
pub struct Environment {
    pub store: HashMap<String, Rc<Object>>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment { store: HashMap::new(), outer: None }
    }

    pub fn new_enclosed(env: Rc<RefCell<Environment>>) -> Environment {
        Environment { store: HashMap::new(), outer: Some(Rc::clone(&env)) }
    }

    pub fn get(&self, name: &str) -> Option<Rc<Object>> {
        match self.store.get(name) {
            Some(obj) => {
                Some(Rc::clone(obj))
            },
            None => {
                match &self.outer {
                    Some(o) => o.borrow().get(name),
                    _ => None,
                }
            },
        }
    }

    pub fn set(&mut self, name: String, obj: Rc<Object>) {
        self.store.insert(name, obj);
    }
}