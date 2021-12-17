use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

use crate::{
    fn_a::Error,
    fn_e::Environment,
    fn_t::{Token, TokenType},
};

use super::{
    func::{CallTrait, Function},
    types::Type,
};

#[derive(Clone)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Function>,
    pub superclass: Option<Box<Class>>,
}

impl Class {
    pub fn new(name: String, methods: Vec<Function>, superclass: Option<Box<Class>>) -> Self {
        let mut map = HashMap::new();
        for method in methods {
            map.insert(method.name.clone(), method);
        }
        Class {
            name,
            methods: map,
            superclass,
        }
    }

    pub fn getfunc(&self, identifier: Token) -> Result<Type, Error> {
        match self.methods.get(&identifier.lexeme) {
            Some(x) => Ok(Type::ClassFunc(x.clone())),
            _ => {
                crate::error(
                    "TypeError",
                    &format!(
                        "`{}` object has no attribute `{}`",
                        self.name,
                        identifier.lexeme.clone()
                    ),
                    identifier.line,
                );
                Err(Error::FromStr("TypeError".into()))
            }
        }
    }

    pub fn a(&self) -> usize {
        match self.methods.get("constructor") {
            Some(f) => f.arity - 1,
            None => 0,
        }
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Class").field("name", &self.name).finish()
    }
}

impl CallTrait for Class {
    fn call(
        &self,
        args: Vec<Type>,
        env: Rc<RefCell<Environment>>,
        callee: Token,
    ) -> Result<Type, Error> {
        if args.len() != self.a() {
            crate::error(
                "TypeError",
                &format!(
                    "`{}()` takes {} positional argument but {} were given",
                    self.name,
                    self.a(),
                    args.len()
                ),
                callee.line,
            );
            return Err(Error::FromStr("Constructor Error".into()));
        }

        let mut fields = HashMap::<String, Box<Type>>::new();
        for (key, value) in &self.methods {
            fields.insert(key.clone(), Box::new(Type::ClassFunc(value.clone())));
        }
        let obj = Object::new(self.clone(), fields);
        if self.methods.get("constructor").is_some() {
            let mut args = args.clone();
            args.insert(0, Type::Object(obj.clone()));
            if let Type::ClassFunc(mut f) = obj.get(&Token::new(
                "constructor".to_string(),
                Type::Null,
                TokenType::Null,
                callee.line,
            ))? {
                f.bind_self(obj, None);
                return f.call(
                    args,
                    Rc::new(RefCell::new(Environment::new(Some(env)))),
                    callee,
                );
            }
        }
        Ok(Type::Object(obj))
    }
}

#[derive(Clone)]
pub struct Object {
    pub class: Class,
    pub fields: HashMap<String, Box<Type>>,
}

impl Object {
    pub fn new(class: Class, fields: HashMap<String, Box<Type>>) -> Self {
        Object { class, fields }
    }

    pub fn get(&self, name: &Token) -> Result<Type, Error> {
        match self.fields.get(&name.lexeme) {
            Some(x) => Ok((**x).clone()),
            None => {
                if let Some(superclass) = &self.class.superclass {
                    return superclass.getfunc(name.clone());
                }
                crate::error(
                    "TypeError",
                    &format!(
                        "`{}` object has no attribute `{}`",
                        self.class.name, name.lexeme
                    ),
                    name.line,
                );
                Err(Error::FromStr(format!(
                    "`{}` object has no attribute `{}`",
                    self.class.name, name
                )))
            }
        }
    }

    pub fn set(&mut self, name: String, value: Type) -> Result<Type, Error> {
        self.fields.insert(name, Box::new(value.clone()));
        Ok(Type::Object(self.clone()))
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Object")
            .field("class", &self.class.name)
            .finish()
    }
}
