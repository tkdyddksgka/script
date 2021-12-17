use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    fn_a::Error,
    fn_e::Environment,
    fn_t::Token,
    fntype::{
        class::{Class, Object},
        n_func::DefaultFunction,
        types::Type,
    },
};

use super::func::CallTrait;

#[derive(Debug, Clone)]
pub enum DefaultClass {
    LIST,
}

impl CallTrait for DefaultClass {
    fn call(
        &self,
        args: Vec<Type>,
        _env: Rc<RefCell<Environment>>,
        _callee: Token,
    ) -> Result<Type, Error> {
        match self {
            DefaultClass::LIST => {
                let l = Rc::new(RefCell::new(args.clone()));
                let c = Class::new("list".to_string(), vec![], None);
                let mut f = HashMap::new();
                f.insert(
                    "get".to_string(),
                    Box::new(Type::DefaultFunc(DefaultFunction::Get(l.clone()))),
                );
                f.insert(
                    "push".to_string(),
                    Box::new(Type::DefaultFunc(DefaultFunction::Push(l.clone()))),
                );
                f.insert(
                    "size".to_string(),
                    Box::new(Type::DefaultFunc(DefaultFunction::Size(l.clone()))),
                );
                f.insert(
                    "remove".to_string(),
                    Box::new(Type::DefaultFunc(DefaultFunction::Remove(l.clone()))),
                );
                f.insert(
                    "set".to_string(),
                    Box::new(Type::DefaultFunc(DefaultFunction::Set(l.clone()))),
                );
                let i = Rc::new(Cell::new(0usize));
                f.insert(
                    "next".to_string(),
                    Box::new(Type::DefaultFunc(DefaultFunction::Next(
                        l.clone(),
                        i.clone(),
                    ))),
                );
                Ok(Type::Object(Object::new(c, f)))
            }
        }
    }
}
