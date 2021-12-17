mod fntype;

mod fn_a;
mod fn_e;
mod fn_i;
mod fn_p;
mod fn_s;
mod fn_t;

use std::{cell::RefCell, env, rc::Rc};

use fn_e::Environment;
use fn_i::interpreter;
use fn_p::Parser;
use fn_s::Scanner;
use fntype::types::Type;

fn run(code: String, env: Rc<RefCell<Environment>>) -> Result<Type, String> {
    let mut s = Scanner::new(code);
    let mut p = Parser::new(s.scan_tokens());
    match interpreter(&p.parse()?, env) {
        Ok(x) => Ok(x),
        Err(e) => Err(e.to_string()),
    }
}

fn error(error: &str, message: &str, line: u64) {
    println!("[{}] Line {}: {}", error, line, message);
}

fn main() {
    run(
        std::fs::read_to_string(&env::args().collect::<Vec<String>>()[1]).unwrap(),
        Rc::new(RefCell::new(Environment::new(None))),
    )
    .unwrap();
}
