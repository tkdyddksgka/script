use core::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub};
use std::cmp::Ordering;
use std::fmt;

use crate::fntype::class::{Class, Object};
use crate::fntype::func::Function;
use crate::fntype::n_class::DefaultClass;
use crate::fntype::n_func::DefaultFunction;

#[derive(Debug, Clone)]
pub enum Type {
    String(String),
    Number(f64),
    Bool(bool),
    Func(Function),
    ClassFunc(Function),
    Class(Class),
    DefaultFunc(DefaultFunction),
    DefaultClass(DefaultClass),
    Object(Object),
    Null,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String(x) => write!(f, "{}", x),
            Type::Number(x) => write!(f, "{}", x),
            Type::Bool(x) => write!(f, "{}", x),
            Type::Func(x) => write!(f, "(Function: {})", x.name),
            Type::DefaultFunc(x) => write!(f, "(Function_d: {:?})", x),
            Type::ClassFunc(x) => write!(f, "(ClassFunc: {})", x.name),
            Type::Class(x) => write!(f, "(Class: {})", x.name),
            Type::DefaultClass(x) => write!(f, "(Class_d: {:?})", x),
            Type::Object(x) => write!(f, "(Object: {})", x.class.name),
            Type::Null => write!(f, "(Null)"),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::Number(a), Type::Number(b)) => a == b,
            (Type::String(a), Type::String(b)) => a == b,
            (Type::Null, Type::Null) => true,
            (Type::Bool(a), Type::Bool(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Type::Number(a), Type::Number(v)) => a.partial_cmp(&v),
            (Type::String(a), Type::String(v)) => a.partial_cmp(&v),
            (Type::Null, Type::Null) => Some(Ordering::Equal),
            (Type::Bool(a), Type::Bool(v)) => a.partial_cmp(&v),
            _ => None,
        }
    }
}

impl Add for Type {
    type Output = Result<Type, String>;

    fn add(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Number(a), Type::Number(b)) => Ok(Type::Number(a + b)),
            (Type::String(a), Type::String(b)) => {
                Ok(Type::String(format!("{}{}", a, b).to_string()))
            }
            _ => Err(String::from("TypeError")),
        }
    }
}

impl Sub for Type {
    type Output = Result<Type, String>;

    fn sub(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Number(a), Type::Number(b)) => Ok(Type::Number(a - b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl Mul for Type {
    type Output = Result<Type, String>;

    fn mul(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Number(a), Type::Number(b)) => Ok(Type::Number(a * b)),
            (Type::Number(a), Type::String(b)) => Ok(Type::String(b.repeat(a as usize))),
            (Type::String(a), Type::Number(b)) => Ok(Type::String(a.repeat(b as usize))),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl Div for Type {
    type Output = Result<Type, String>;

    fn div(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Number(a), Type::Number(b)) => {
                if b == 0.0 {
                    Err(String::from("ZeroDivisionError"))
                } else {
                    Ok(Type::Number(a / b))
                }
            }
            _ => Err(String::from("TypeError")),
        }
    }
}

impl Rem for Type {
    type Output = Result<Type, String>;

    fn rem(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Number(a), Type::Number(b)) => {
                if b == 0.0 {
                    Err(String::from("ZeroDivisionError"))
                } else {
                    Ok(Type::Number(a % b))
                }
            }
            _ => Err(String::from("TypeError")),
        }
    }
}

impl BitOr for Type {
    type Output = Result<Type, String>;

    fn bitor(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Bool(a), Type::Bool(b)) => Ok(Type::Bool(a | b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl BitAnd for Type {
    type Output = Result<Type, String>;

    fn bitand(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Bool(a), Type::Bool(b)) => Ok(Type::Bool(a & b)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl BitXor for Type {
    type Output = Result<Type, String>;

    fn bitxor(self, right: Type) -> Result<Type, String> {
        match (self, right) {
            (Type::Bool(x), Type::Bool(y)) => Ok(Type::Bool(x ^ y)),
            _ => Err(String::from("TypeError")),
        }
    }
}

impl Type {
    pub fn type_(self) -> String {
        match self {
            Type::Number(_) => "number",
            Type::String(_) => "string",
            Type::Bool(_) => "bool",
            Type::Func(_) | Type::DefaultFunc(_) => "function",
            Type::ClassFunc(_) => "classfunc",
            Type::Class(_) | Type::DefaultClass(_) => "class",
            Type::Object(_) => "object",
            Type::Null => "null",
        }
        .to_string()
    }
}
