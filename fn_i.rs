use std::cell::RefCell;
use std::rc::Rc;

use crate::fn_a::{Error, Eval};
use crate::fn_e::Environment;
use crate::fntype::types::Type;

pub fn interpreter(
    statements: &Vec<Box<dyn Eval>>,
    env: Rc<RefCell<Environment>>,
) -> Result<Type, Error> {
    let mut error_count: u32 = 0;
    let mut res = Type::Null;

    for stmt in statements {
        match stmt.eval(env.clone()) {
            Err(e) => {
                match e {
                    Error::Break | Error::Continue => return Err(e),
                    Error::Return(_) => return Err(e),
                    _ => {}
                }
                error_count += 1;
            }
            Ok(x) => res = x,
        }
    }
    if error_count > 0 {
        Err(Error::FromStr(format!(
            "RuntimeError: {} errors",
            error_count
        )))
    } else {
        Ok(res)
    }
}
