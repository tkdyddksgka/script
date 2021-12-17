use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    fn_a::{BlockStmt, Error},
    fn_e::Environment,
    fn_i::interpreter,
    fn_t::{Token, TokenType},
};

use super::{class::Object, types::Type};

pub trait CallTrait {
    fn call(
        &self,
        args: Vec<Type>,
        env: Rc<RefCell<Environment>>,
        callee: Token,
    ) -> Result<Type, Error>;
}

#[derive(Clone)]
pub struct Function {
    pub self_: Option<Object>,
    pub self_id: Option<String>,
    pub arity: usize,
    pub address: u64,
    pub name: String,
    pub body: Rc<RefCell<BlockStmt>>,
    pub args: Vec<String>,
}

impl Function {
    pub fn new(
        name: String,
        address: u64,
        body: Rc<RefCell<BlockStmt>>,
        args: Vec<String>,
    ) -> Self {
        Function {
            self_: None,
            self_id: None,
            arity: args.len(),
            name,
            address,
            body,
            args,
        }
    }

    pub fn bind_self(&mut self, obj: Object, obj_id: Option<String>) {
        self.self_ = Some(obj);
        self.self_id = obj_id;
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Function")
            .field("identifier", &self.name)
            .field("address", &self.address)
            .finish()
    }
}

impl CallTrait for Function {
    fn call(
        &self,
        args: Vec<Type>,
        env: Rc<RefCell<Environment>>,
        callee: Token,
    ) -> Result<Type, Error> {
        if args.len() != self.arity {
            crate::error(
                "TypeError",
                &format!(
                    "`{}()` takes {} positional argument but {} were given",
                    self.name,
                    self.arity,
                    args.len()
                ),
                callee.line,
            );
            return Err(Error::FromStr("Error".into()));
        }

        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env))));

        for (identifier, value) in self.args.iter().zip(args.iter()) {
            let t = Token::new(
                identifier.clone(),
                Type::Null,
                TokenType::Ident,
                self.address,
            );
            enclosing.borrow_mut().add(t, value.clone())?;
        }
        if let Some(id) = &self.self_id {
            let t = Token::new(
                self.args[0].clone(),
                Type::Null,
                TokenType::Ident,
                self.address,
            );
            let value = enclosing.borrow().get(t)?;
            let id = Token::new(id.to_string(), Type::Null, TokenType::Ident, self.address);
            enclosing
                .borrow_mut()
                .en
                .as_ref()
                .unwrap()
                .borrow_mut()
                .assign(id, value)?;
        }
        match interpreter(&(self.body.borrow().statements), enclosing.clone()) {
            Ok(x) => Ok(x),
            Err(e) => match e {
                Error::Return(x) => Ok(x),
                _ => Err(e),
            },
        }
    }
}
