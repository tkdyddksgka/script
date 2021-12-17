use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use crate::{fn_a::Error, fn_e::Environment, fn_t::Token};

use super::{func::CallTrait, types::Type};

#[derive(Debug, Clone)]
pub enum DefaultFunction {
    Read,
    Get(Rc<RefCell<Vec<Type>>>),
    Push(Rc<RefCell<Vec<Type>>>),
    Size(Rc<RefCell<Vec<Type>>>),
    Remove(Rc<RefCell<Vec<Type>>>),
    Set(Rc<RefCell<Vec<Type>>>),
    Next(Rc<RefCell<Vec<Type>>>, Rc<Cell<usize>>),
}

impl CallTrait for DefaultFunction {
    fn call(
        &self,
        args: Vec<Type>,
        _: Rc<RefCell<Environment>>,
        call: Token,
    ) -> Result<Type, Error> {
        match self {
            DefaultFunction::Read => {
                if args.len() != 0 {
                    crate::error(
                        "TypeError",
                        &format!(
                            "`read()` takes 0 positional argument but {} were given",
                            args.len()
                        ),
                        call.line,
                    );
                    return Err(Error::FromStr("Error".into()));
                }
                let mut input = String::new();
                std::io::stdin().read_line(&mut input).unwrap();
                match input.trim().parse::<f64>() {
                    Ok(x) => return Ok(Type::Number(x)),
                    Err(_) => return Ok(Type::String(input.trim().to_string())),
                }
            }
            DefaultFunction::Get(list) => {
                if args.len() == 1 {
                    match &args[0] {
                        Type::Number(x) => {
                            let index = *x as usize;
                            if list.borrow().len() <= index {
                                crate::error("IndexError", "IndexOutOfRange", call.line);
                                return Err(Error::FromStr("IndexError".into()));
                            }
                            return Ok(list.borrow()[index].clone());
                        }
                        _ => {
                            crate::error("TypeError", "Invalid Index", call.line);
                            return Err(Error::FromStr("TypeError".into()));
                        }
                    }
                } else {
                    crate::error(
                        "TypeError",
                        &format!(
                            "`get` takes 1 positional argument but {} were given",
                            args.len()
                        ),
                        call.line,
                    );
                    return Err(Error::FromStr("Error".into()));
                }
            }
            DefaultFunction::Push(list) => {
                if args.len() == 1 {
                    list.borrow_mut().push(args[0].clone());
                    return Ok(Type::Null);
                } else {
                    crate::error(
                        "TypeError",
                        &format!(
                            "`push()` takes 1 positional argument but {} were given",
                            args.len()
                        ),
                        call.line,
                    );
                    return Err(Error::FromStr("Error".into()));
                }
            }
            DefaultFunction::Size(list) => {
                if args.len() == 0 {
                    return Ok(Type::Number(list.borrow_mut().len() as f64));
                } else {
                    crate::error(
                        "TypeError",
                        &format!(
                            "`size()` takes 0 positional argument but {} were given",
                            args.len()
                        ),
                        call.line,
                    );
                    return Err(Error::FromStr("Error".into()));
                }
            }
            DefaultFunction::Remove(list) => {
                if args.len() == 1 {
                    match &args[0] {
                        Type::Number(x) => {
                            let index = *x as usize;
                            if list.borrow().len() <= index {
                                crate::error("IndexError", "IndexOutOfRange", call.line);
                                return Err(Error::FromStr("IndexError".into()));
                            }
                            let ret = list.borrow_mut().remove(index);
                            return Ok(ret);
                        }
                        _ => {
                            crate::error("TypeError", "Invalid Index", call.line);
                            return Err(Error::FromStr("TypeError".into()));
                        }
                    }
                } else {
                    crate::error(
                        "TypeError",
                        &format!(
                            "`remove()` takes 1 positional argument but {} were given",
                            args.len()
                        ),
                        call.line,
                    );
                    return Err(Error::FromStr("Error".into()));
                }
            }
            DefaultFunction::Set(list) => {
                if args.len() == 2 {
                    match &args[0] {
                        Type::Number(x) => {
                            let index = *x as usize;
                            if list.borrow().len() <= index {
                                crate::error("IndexError", "IndexOutOfRange", call.line);
                                return Err(Error::FromStr("IndexError".into()));
                            }
                            list.borrow_mut()[index] = args[1].clone();
                            return Ok(Type::Null);
                        }
                        _ => {
                            crate::error("TypeError", "Invalid Index", call.line);
                            return Err(Error::FromStr("TypeError".into()));
                        }
                    }
                } else {
                    crate::error(
                        "TypeError",
                        &format!(
                            "`set()` takes 2 positional argument but {} were given",
                            args.len()
                        ),
                        call.line,
                    );
                    return Err(Error::FromStr("Errror".into()));
                }
            }
            DefaultFunction::Next(list, i) => {
                if args.len() == 0 {
                    let index = i.get();
                    if index == list.borrow().len() {
                        i.set(0);
                        return Err(Error::Iteration);
                    }
                    i.set(index + 1);
                    return Ok(list.borrow()[index].clone());
                } else {
                    crate::error(
                        "TypeError",
                        &format!(
                            "`next()` takes 0 positional argument but {} were given",
                            args.len()
                        ),
                        call.line,
                    );
                    return Err(Error::FromStr("function arity error".into()));
                }
            }
        }
    }
}
