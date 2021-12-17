use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::fn_a::Error;
use crate::fn_t::Token;
use crate::fntype::n_class::DefaultClass;
use crate::fntype::n_func::DefaultFunction;
use crate::fntype::types::Type;

fn add_natives(env: &mut Environment) {
    env.symbol
        .insert("Read".to_string(), Type::DefaultFunc(DefaultFunction::Read));
    env.symbol
        .insert("Array".to_string(), Type::DefaultClass(DefaultClass::LIST));
}

#[derive(Debug)]
pub struct Environment {
    pub en: Option<Rc<RefCell<Environment>>>,
    pub symbol: HashMap<String, Type>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Self {
        let symbol_table: HashMap<String, Type> = HashMap::new();
        if enclosing.is_none() {
            let mut env = Environment {
                en: enclosing,
                symbol: symbol_table,
            };
            add_natives(&mut env);
            return env;
        }
        Environment {
            en: enclosing,
            symbol: symbol_table,
        }
    }

    pub fn add(&mut self, identifier: Token, value: Type) -> Result<(), Error> {
        self.symbol.insert(identifier.lexeme, value);
        Ok(())
    }

    pub fn assign(&mut self, identifier: Token, value: Type) -> Result<Type, Error> {
        let name = &identifier.lexeme;
        if self.symbol.contains_key(name) {
            self.symbol.insert(identifier.lexeme, value.clone());
            return Ok(value);
        } else {
            match &mut self.en {
                Some(env) => env.borrow_mut().assign(identifier.clone(), value.clone()),
                None => {
                    crate::error(
                        "NameError",
                        &format!("Uninitialized Variable: `{}`", name),
                        identifier.line,
                    );
                    Err(Error::FromStr(
                        format!("Uninitialized Variable: `{}`", name).into(),
                    ))
                }
            }
        }
    }

    pub fn get(&self, identifier: Token) -> Result<Type, Error> {
        let name = &identifier.lexeme;
        match self.symbol.get(name) {
            Some(x) => Ok(x.clone()),
            None => match &self.en {
                Some(env) => env.borrow().get(identifier.clone()),
                None => {
                    crate::error(
                        "NameError",
                        &format!("Undefined Variable: `{}`", name),
                        identifier.line,
                    );
                    Err(Error::FromStr(
                        format!("Undefined Variable: `{}`", name).into(),
                    ))
                }
            },
        }
    }
}
