use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::fn_e::Environment;
use crate::fn_i::interpreter;
use crate::fn_t::{Token, TokenType};
use crate::fntype::class::{Class, Object};
use crate::fntype::func::{CallTrait, Function};
use crate::fntype::types::Type;

#[derive(Debug)]
pub enum Error {
    FromStr(String),
    Break,
    Continue,
    Return(Type),
    Iteration,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::FromStr(x) => write!(f, "{}", x),
            Error::Break => write!(f, "Continue"),
            Error::Continue => write!(f, "Break"),
            Error::Return(x) => write!(f, "Return {}", x),
            Error::Iteration => write!(f, "Stop Iteration"),
        }
    }
}

pub trait Eval {
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error>;

    fn get_type(&self) -> String;
}

pub struct Binary {
    left: Box<dyn Eval>,
    right: Box<dyn Eval>,
    operator: Token,
}

pub struct Unary {
    expression: Box<dyn Eval>,
    operator: Token,
}

pub struct Literal {
    value: Type,
}

pub struct Grouping {
    expression: Box<dyn Eval>,
}

pub struct Variable {
    identifier: Token,
}

pub struct Assignment {
    identifier: Token,
    value: Box<dyn Eval>,
}

impl Eval for Binary {
    fn get_type(&self) -> String {
        String::from("BinExpr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let o = &self.operator.lexeme;
        let l = self.left.eval(env.clone())?;
        let r = self.right.eval(env.clone())?;

        match &**o {
            "==" => Ok(Type::Bool(l == r)),
            "!=" => Ok(Type::Bool(l != r)),
            ">=" => Ok(Type::Bool(l >= r)),
            "<=" => Ok(Type::Bool(l <= r)),
            ">" => Ok(Type::Bool(l > r)),
            "<" => Ok(Type::Bool(l < r)),
            "+" => {
                let res = l.clone() + r.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!(
                            "unsupported operand type(s) for +: {} and {}",
                            l.type_(),
                            r.type_()
                        );
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(Error::FromStr(error_message.into()))
                    }
                }
            }
            "-" => {
                let res = l.clone() - r.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!(
                            "unsupported operand type(s) for -: {} and {}",
                            l.type_(),
                            r.type_()
                        );
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(Error::FromStr(error_message.into()))
                    }
                }
            }
            "/" => {
                let res = l.clone() / r.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        if x == String::from("ZeroDivisionError") {
                            crate::error(
                                "ZeroDivisionError",
                                "division by zero",
                                self.operator.line,
                            );
                            Err(Error::FromStr("ZeroDivisionError".into()))
                        } else {
                            let error_message = format!(
                                "unsupported operand type(s) for /: {} and {}",
                                l.type_(),
                                r.type_()
                            );
                            crate::error("TypeError", &error_message, self.operator.line);
                            Err(Error::FromStr(error_message.into()))
                        }
                    }
                }
            }
            "%" => {
                let res = l.clone() % r.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(x) => {
                        if x == String::from("ZeroDivisionError") {
                            crate::error(
                                "ZeroDivisionError",
                                "modulo division by zero",
                                self.operator.line,
                            );
                            Err(Error::FromStr("ZeroDivisionError".into()))
                        } else {
                            let error_message = format!(
                                "unsupported operand type(s) for %: {} and {}",
                                l.type_(),
                                r.type_()
                            );
                            crate::error("TypeError", &error_message, self.operator.line);
                            Err(Error::FromStr(error_message.into()))
                        }
                    }
                }
            }
            "*" => {
                let res = l.clone() * r.clone();
                match res {
                    Ok(x) => Ok(x),
                    Err(_) => {
                        let error_message = format!(
                            "Invalid operand type(s) for *: {}, {}",
                            l.type_(),
                            r.type_()
                        );
                        crate::error("TypeError", &error_message, self.operator.line);
                        Err(Error::FromStr(error_message.into()))
                    }
                }
            }
            _ => {
                crate::error(
                    "Parsing Error",
                    &format!("unsupported binary operator: {}", o.clone()),
                    self.operator.line,
                );
                Err(Error::FromStr(
                    format!("unsupported binary operator: {}", o).into(),
                ))
            }
        }
    }
}

impl Eval for Unary {
    fn get_type(&self) -> String {
        String::from("UnaryExpr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let op = &self.operator.lexeme;
        let expr = self.expression.eval(env.clone())?;
        match &**op {
            "-" => match expr {
                Type::Number(x) => Ok(Type::Number(-x)),
                _ => {
                    let error_message = format!("Invalid right expression: `{}`", op);
                    crate::error("ParseError", &error_message, self.operator.line);
                    Err(Error::FromStr(error_message.into()))
                }
            },
            "!" => match expr {
                Type::Bool(x) => Ok(Type::Bool(!x)),
                _ => {
                    let error_message = format!("Invalid right expression: `{}`", op);
                    crate::error("ParseError", &error_message, self.operator.line);
                    Err(Error::FromStr(error_message.into()))
                }
            },
            _ => {
                crate::error(
                    "Parsing error",
                    &format!("Invalid unary operator: `{}`", op.clone()),
                    self.operator.line,
                );
                Err(Error::FromStr(
                    format!("Invalid unary operator: `{}`", op).into(),
                ))
            }
        }
    }
}

impl Eval for Literal {
    fn get_type(&self) -> String {
        String::from("Literal")
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        Ok(self.value.clone())
    }
}

impl Eval for Grouping {
    fn get_type(&self) -> String {
        String::from("Grouping")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        self.expression.eval(env)
    }
}

impl Eval for Variable {
    fn get_type(&self) -> String {
        format!("Variable: {}", self.identifier.lexeme)
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        env.borrow().get(self.identifier.clone())
    }
}

impl Eval for Assignment {
    fn get_type(&self) -> String {
        String::from("Assignment")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let value = self.value.eval(env.clone())?;
        env.borrow_mut()
            .assign(self.identifier.clone(), value.clone())
    }
}

impl Binary {
    pub fn new(operator: Token, left: Box<dyn Eval>, right: Box<dyn Eval>) -> Option<Self> {
        match &operator.t {
            TokenType::EqEq
            | TokenType::BangEq
            | TokenType::Less
            | TokenType::LessEq
            | TokenType::Greater
            | TokenType::GreaterEq
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Slash
            | TokenType::Percent => Some(Binary {
                operator,
                left,
                right,
            }),
            _ => {
                crate::error(
                    "ParseError",
                    &format!("Unexpected operator: {}", operator.lexeme),
                    operator.line,
                );
                None
            }
        }
    }
}

impl Unary {
    pub fn new(operator: Token, expression: Box<dyn Eval>) -> Option<Unary> {
        match &operator.t {
            TokenType::Minus | TokenType::Bang => Some(Unary {
                operator,
                expression,
            }),
            _ => {
                crate::error(
                    "ParseError",
                    &format!("Unexpected operator: {}", operator.lexeme),
                    operator.line,
                );
                None
            }
        }
    }
}

impl Grouping {
    pub fn new(expression: Box<dyn Eval>) -> Grouping {
        Grouping { expression }
    }
}

impl Literal {
    pub fn new(token: Token) -> Option<Literal> {
        match &token.t {
            TokenType::Num
            | TokenType::Str
            | TokenType::Null
            | TokenType::True
            | TokenType::False => Some(Literal {
                value: token.literal,
            }),
            _ => {
                crate::error(
                    "ParseError",
                    &format!("Unexpected value: {}", token.lexeme),
                    token.line,
                );
                None
            }
        }
    }
}

impl Variable {
    pub fn new(identifier: Token) -> Variable {
        Variable { identifier }
    }
}

impl Assignment {
    pub fn new(identifier: Token, value: Box<dyn Eval>) -> Self {
        Assignment { identifier, value }
    }
}

pub struct LogicalExpr {
    operator: Token,
    left: Box<dyn Eval>,
    right: Box<dyn Eval>,
}

impl LogicalExpr {
    pub fn new(operator: Token, left: Box<dyn Eval>, right: Box<dyn Eval>) -> Result<Self, Error> {
        match &operator.t {
            TokenType::Or | TokenType::And | TokenType::Xor => {}
            _ => {
                crate::error(
                    "SyntaxError",
                    &format!("Invalid logical operator: `{}`", operator),
                    operator.line,
                );
                return Err(Error::FromStr(format!(
                    "SyntaxError: Invalid logical operator: `{}`",
                    operator
                )));
            }
        }
        Ok(LogicalExpr {
            operator,
            left,
            right,
        })
    }
}

impl Eval for LogicalExpr {
    fn get_type(&self) -> String {
        String::from("LogicalExpr")
    }
    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let left = self.left.eval(env.clone())?;
        let right = self.right.eval(env.clone())?;
        let op = &self.operator.t;

        match &op {
            TokenType::Or => match left.clone() | right.clone() {
                Ok(x) => Ok(x),
                Err(_) => {
                    let error_message = format!(
                        "unsupported operand type(s) for `or`: {} and {}",
                        left.type_(),
                        right.type_()
                    );
                    crate::error("TypeError", &error_message, self.operator.line);
                    Err(Error::FromStr(error_message.into()))
                }
            },
            TokenType::And => match left.clone() & right.clone() {
                Ok(x) => Ok(x),
                Err(_) => {
                    let error_message = format!(
                        "unsupported operand type(s) for `and`: {} and {}",
                        left.type_(),
                        right.type_()
                    );
                    crate::error("TypeError", &error_message, self.operator.line);
                    Err(Error::FromStr(error_message.into()))
                }
            },
            TokenType::Xor => match left.clone() ^ right.clone() {
                Ok(x) => Ok(x),
                Err(_) => {
                    let error_message = format!(
                        "unsupported operand type(s) for `xor`: {} and {}",
                        left.type_(),
                        right.type_()
                    );
                    crate::error("TypeError", &error_message, self.operator.line);
                    Err(Error::FromStr(error_message.into()))
                }
            },
            _ => {
                crate::error(
                    "SyntaxError",
                    &format!("Invalid logical operator: `{}`", self.operator),
                    self.operator.line,
                );
                Err(Error::FromStr(
                    format!("SyntaxError: invalid operator: `{}`", self.operator).into(),
                ))
            }
        }
    }
}

pub struct Call {
    identifier: Box<dyn Eval>,
    paren: Token,
    arguments: Vec<Box<dyn Eval>>,
}

impl Call {
    pub fn new(identifier: Box<dyn Eval>, paren: Token, arguments: Vec<Box<dyn Eval>>) -> Self {
        Call {
            identifier,
            paren,
            arguments,
        }
    }
}

impl Eval for Call {
    fn get_type(&self) -> String {
        String::from("Call")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let mut args: Vec<Type> = Vec::new();

        for arg in &self.arguments {
            args.push(arg.eval(env.clone())?);
        }
        let callable: Box<dyn CallTrait> = match self.identifier.eval(env.clone()) {
            Ok(obj) => match obj {
                Type::Func(f) => Box::new(f),
                Type::Class(c) => Box::new(c),
                Type::DefaultClass(c) => Box::new(c),
                Type::DefaultFunc(f) => Box::new(f),
                Type::ClassFunc(m) => {
                    if let Some(obj) = &m.self_ {
                        args.insert(0, Type::Object(obj.clone()));
                    }
                    Box::new(m)
                }
                _ => {
                    crate::error(
                        "TypeError",
                        &format!("`{}` object is not callable", obj.type_()),
                        self.paren.line,
                    );
                    return Err(Error::FromStr("TypeError".into()));
                }
            },
            Err(e) => return Err(e),
        };
        (*callable).call(args, env.clone(), self.paren.clone())
    }
}

pub struct Get {
    pub name: Token,
    object: Box<dyn Eval>,
}

impl Get {
    pub fn new(name: Token, object: Box<dyn Eval>) -> Self {
        Get { name, object }
    }
}

impl Eval for Get {
    fn get_type(&self) -> String {
        String::from("Getter")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match self.object.eval(env.clone()) {
            Ok(obj) => match obj {
                Type::Object(x) => match x.get(&self.name)? {
                    Type::ClassFunc(mut f) => {
                        if self.object.get_type().contains("Variable") {
                            f.bind_self(x, Some(self.object.get_type()[10..].to_string()));
                        } else {
                            f.bind_self(x, None);
                        }
                        Ok(Type::ClassFunc(f))
                    }
                    _ => x.get(&self.name),
                },
                Type::Class(x) => x.getfunc(self.name.clone()),
                _ => {
                    crate::error(
                        "TypeError",
                        &format!("`{}` object has no attributes", obj.type_()),
                        self.name.line,
                    );
                    Err(Error::FromStr("TypeError".into()))
                }
            },
            Err(e) => Err(e),
        }
    }
}

pub struct Set {
    name: Token,
    object_id: Token,
    object: Box<dyn Eval>,
    value: Box<dyn Eval>,
}

impl Set {
    pub fn new(name: Token, object: Box<dyn Eval>, value: Box<dyn Eval>) -> Self {
        let object_id = &object.get_type()[10..];
        let object_id = Token::new(
            object_id.to_string(),
            Type::String(object_id.to_string()),
            TokenType::Ident,
            name.line,
        );
        Set {
            name,
            object_id,
            object,
            value,
        }
    }
}

impl Eval for Set {
    fn get_type(&self) -> String {
        String::from("Setter")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match self.object.eval(env.clone()) {
            Ok(obj) => match obj {
                Type::Object(mut x) => {
                    let value = self.value.eval(env.clone())?;
                    env.borrow_mut().assign(
                        self.object_id.clone(),
                        x.set(self.name.lexeme.clone(), value.clone())?,
                    )?;
                    Ok(value)
                }
                _ => {
                    crate::error(
                        "TypeError",
                        &format!("`{}` object has no attributes", obj.type_()),
                        self.name.line,
                    );
                    return Err(Error::FromStr("TypeError".into()));
                }
            },
            Err(e) => return Err(e),
        }
    }
}

pub struct ExprStmt {
    body: Box<dyn Eval>,
}

impl ExprStmt {
    pub fn new(expr: Box<dyn Eval>) -> ExprStmt {
        ExprStmt { body: expr }
    }
}

impl Eval for ExprStmt {
    fn get_type(&self) -> String {
        String::from("ExprStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match self.body.eval(env) {
            Err(e) => Err(e),
            Ok(res) => Ok(res),
        }
    }
}

pub struct PrintStmt {
    value: Box<dyn Eval>,
}

impl PrintStmt {
    pub fn new(expr: Box<dyn Eval>) -> PrintStmt {
        PrintStmt { value: expr }
    }
}

impl Eval for PrintStmt {
    fn get_type(&self) -> String {
        String::from("PrintStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match self.value.eval(env) {
            Ok(val) => {
                print!("{}", val);
                Ok(Type::Null)
            }
            Err(e) => Err(e),
        }
    }
}

pub struct PrintLnStmt {
    value: Box<dyn Eval>,
}

impl PrintLnStmt {
    pub fn new(expr: Box<dyn Eval>) -> PrintLnStmt {
        PrintLnStmt { value: expr }
    }
}

impl Eval for PrintLnStmt {
    fn get_type(&self) -> String {
        String::from("PrintLnStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match self.value.eval(env) {
            Ok(val) => {
                println!("{}", val);
                Ok(Type::Null)
            }
            Err(e) => Err(e),
        }
    }
}

pub struct VarDeclaration {
    identifier: Token,
    value: Box<dyn Eval>,
}

impl VarDeclaration {
    pub fn new(identifier: Token, value: Box<dyn Eval>) -> VarDeclaration {
        VarDeclaration { identifier, value }
    }
}

impl Eval for VarDeclaration {
    fn get_type(&self) -> String {
        String::from("VarDeclr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let value = self.value.eval(env.clone());
        match value {
            Ok(x) => match env.borrow_mut().add(self.identifier.clone(), x) {
                Err(e) => Err(e),
                Ok(_) => Ok(Type::Null),
            },
            Err(e) => Err(e),
        }
    }
}

pub struct BlockStmt {
    pub statements: Vec<Box<dyn Eval>>,
}

impl BlockStmt {
    pub fn new(statements: Vec<Box<dyn Eval>>) -> Self {
        BlockStmt { statements }
    }
}

impl Eval for BlockStmt {
    fn get_type(&self) -> String {
        String::from("BlockStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env))));
        interpreter(&self.statements, enclosing)
    }
}

pub struct IfStmt {
    condition: Box<dyn Eval>,
    expr: Box<dyn Eval>,
    else_stmt: Option<Box<dyn Eval>>,
}

impl IfStmt {
    pub fn new(
        condition: Box<dyn Eval>,
        expr: Box<dyn Eval>,
        else_stmt: Option<Box<dyn Eval>>,
    ) -> Self {
        IfStmt {
            condition,
            expr,
            else_stmt,
        }
    }
}

impl Eval for IfStmt {
    fn get_type(&self) -> String {
        String::from("IfStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        if self.condition.eval(env.clone()).unwrap() == Type::Bool(true) {
            return self.expr.eval(env);
        }
        match &self.else_stmt {
            Some(stmt) => stmt.eval(env),
            None => Ok(Type::Null),
        }
    }
}

pub struct WhileLoop {
    condition: Box<dyn Eval>,
    body: BlockStmt,
}

impl WhileLoop {
    pub fn new(condition: Box<dyn Eval>, body: BlockStmt) -> Self {
        WhileLoop { condition, body }
    }
}

impl Eval for WhileLoop {
    fn get_type(&self) -> String {
        String::from("WhileLoop")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let mut ret: Result<Type, Error>;
        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));

        while self.condition.eval(env.clone())? == Type::Bool(true) {
            for stmt in &self.body.statements {
                ret = stmt.eval(enclosing.clone());
                match ret {
                    Ok(_) => {}
                    Err(e) => match e {
                        Error::Break => return Ok(Type::Null),
                        Error::Continue => break,
                        _ => return Err(e),
                    },
                }
            }
        }
        Ok(Type::Null)
    }
}

pub struct ForLoop {
    alias: Token,
    iterable: Box<dyn Eval>,
    body: BlockStmt,
}

impl ForLoop {
    pub fn new(alias: Token, iterable: Box<dyn Eval>, body: BlockStmt) -> Self {
        ForLoop {
            alias,
            iterable,
            body,
        }
    }
}

impl Eval for ForLoop {
    fn get_type(&self) -> String {
        String::from("ForLoop")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let mut ret: Result<Type, Error>;

        let enclosing = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));

        let t = Token::new(
            "next".to_string(),
            Type::Null,
            TokenType::Null,
            self.alias.line,
        );

        let iterable = self.iterable.eval(env.clone())?;
        let iter = match iterable {
            Type::Object(obj) => obj,
            _ => {
                crate::error(
                    "TypeError",
                    &format!("`{}` does not implement iterator", iterable.type_()),
                    self.alias.line,
                );
                return Err(Error::FromStr("TypeError".into()));
            }
        };

        loop {
            if let Type::DefaultFunc(f) = iter.get(&t)? {
                let argv: Vec<Type> = vec![];
                let alias_value = f.call(argv, env.clone(), self.alias.clone());
                match alias_value {
                    Err(e) => {
                        if let Error::Iteration = e {
                            break;
                        }
                    }
                    Ok(value) => {
                        enclosing.borrow_mut().add(self.alias.clone(), value)?;
                        for stmt in &self.body.statements {
                            ret = stmt.eval(enclosing.clone());
                            match ret {
                                Ok(_) => {}
                                Err(e) => match e {
                                    Error::Break => return Ok(Type::Null),
                                    Error::Continue => break,
                                    _ => return Err(e),
                                },
                            }
                        }
                    }
                }
            }
        }
        Ok(Type::Null)
    }
}

pub struct Break {}

impl Break {
    pub fn new() -> Self {
        Break {}
    }
}

impl Eval for Break {
    fn get_type(&self) -> String {
        String::from("BreakStmt")
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        Err(Error::Break)
    }
}

pub struct Continue {}

impl Continue {
    pub fn new() -> Self {
        Continue {}
    }
}

impl Eval for Continue {
    fn get_type(&self) -> String {
        String::from("ContinueStmt")
    }

    fn eval(&self, _env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        Err(Error::Continue)
    }
}

pub struct FunDeclaration {
    identifier: Token,
    pub f: Function,
}

impl FunDeclaration {
    pub fn new(identifier: Token, args: Vec<Token>, body: BlockStmt) -> Self {
        let args: Vec<String> = args.iter().map(|x| x.lexeme.clone()).collect();
        let f = Function::new(
            identifier.lexeme.clone(),
            identifier.line,
            Rc::new(RefCell::new(body)),
            args,
        );
        FunDeclaration { identifier, f }
    }
}

impl Eval for FunDeclaration {
    fn get_type(&self) -> String {
        String::from("FunDeclr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        env.borrow_mut()
            .add(self.identifier.clone(), Type::Func(self.f.clone()))?;
        Ok(Type::Null)
    }
}

pub struct Return {
    expr: Box<dyn Eval>,
}

impl Return {
    pub fn new(expr: Box<dyn Eval>) -> Self {
        Return { expr }
    }
}

impl Eval for Return {
    fn get_type(&self) -> String {
        String::from("ReturnStmt")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        Err(Error::Return(self.expr.eval(env.clone())?))
    }
}

pub struct ClassDeclr {
    name: Token,
    methods: Vec<Function>,
    superclass: Option<Token>,
}

impl ClassDeclr {
    pub fn new(name: Token, superclass: Option<Token>, methods: Vec<Function>) -> Self {
        ClassDeclr {
            name,
            superclass,
            methods,
        }
    }
}

impl Eval for ClassDeclr {
    fn get_type(&self) -> String {
        String::from("ClassDeclr")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match &self.superclass {
            Some(s) => {
                let superclass = env.borrow().get(s.clone())?;
                match superclass {
                    Type::Class(x) => {
                        let class = Class::new(
                            self.name.lexeme.clone(),
                            self.methods.clone(),
                            Some(Box::new(x.clone())),
                        );
                        env.borrow_mut()
                            .add(self.name.clone(), Type::Class(class.clone()))?;
                        Ok(Type::Null)
                    }
                    _ => {
                        crate::error(
                            "TypeError",
                            &format!("Cannot Inherit From `{}`", superclass.type_()),
                            self.name.line,
                        );
                        Err(Error::FromStr("TypeError".into()))
                    }
                }
            }
            None => {
                let class = Class::new(self.name.lexeme.clone(), self.methods.clone(), None);
                env.borrow_mut()
                    .add(self.name.clone(), Type::Class(class.clone()))?;
                Ok(Type::Null)
            }
        }
    }
}

pub struct Import {
    module: Token,
    item: Option<Token>,
    alias: Option<Token>,
}

impl Import {
    pub fn new(module: Token, item: Option<Token>, alias: Option<Token>) -> Self {
        Import {
            module,
            item,
            alias,
        }
    }
}

impl Eval for Import {
    fn get_type(&self) -> String {
        String::from("Import")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        match std::fs::read_to_string(format!("{}.fn", self.module.lexeme).clone()) {
            Ok(code) => {
                let methods: Vec<Function> = Vec::new();
                let class = Class::new(String::from("__FnScriptModule"), methods, None);
                let t = Token::new(
                    String::from("__FnScriptModule"),
                    Type::Null,
                    TokenType::Null,
                    self.module.line,
                );
                env.borrow_mut().add(t, Type::Class(class.clone()))?;

                let module_env = Rc::new(RefCell::new(Environment::new(None)));
                match crate::run(code, module_env.clone()) {
                    Ok(_) => {}
                    Err(e) => {
                        return Err(Error::FromStr(e));
                    }
                }
                match &self.item {
                    None => {
                        let mut fields: HashMap<String, Box<Type>> = HashMap::new();
                        for (identifier, value) in module_env.borrow().symbol.iter() {
                            fields.insert(identifier.clone(), Box::new(value.clone()));
                        }
                        let object = Type::Object(Object::new(class, fields));
                        match &self.alias {
                            Some(t) => {
                                env.borrow_mut().add(t.clone(), object)?;
                            }
                            _ => {
                                let t = Token::new(
                                    self.module.lexeme.clone(),
                                    Type::Null,
                                    TokenType::Null,
                                    self.module.line,
                                );
                                env.borrow_mut().add(t, object)?;
                            }
                        }
                    }
                    Some(t) => match &self.alias {
                        Some(a) => {
                            let value = module_env.borrow().get(t.clone())?;
                            env.borrow_mut().add(a.clone(), value)?;
                        }
                        None => {
                            let value = module_env.borrow().get(t.clone())?;
                            env.borrow_mut().add(t.clone(), value)?;
                        }
                    },
                }
                Ok(Type::Null)
            }
            Err(_) => {
                crate::error(
                    "ModuleNotFound",
                    &format!("File Not Found: {}", format!("{}", self.module.lexeme)),
                    self.module.line,
                );
                Err(Error::FromStr("ModuleNotFound".into()))
            }
        }
    }
}

pub struct Index {
    iterable: Box<dyn Eval>,
    index: Box<dyn Eval>,
    operator: Token,
}

impl Index {
    pub fn new(iterable: Box<dyn Eval>, operator: Token, index: Box<dyn Eval>) -> Self {
        Index {
            iterable,
            index,
            operator,
        }
    }
}

impl Eval for Index {
    fn get_type(&self) -> String {
        String::from("Index")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let argv: Vec<Type> = vec![self.index.eval(env.clone())?];
        let t = Token::new(
            "index".to_string(),
            Type::Null,
            TokenType::Null,
            self.operator.line,
        );
        let iterable = self.iterable.eval(env.clone())?;
        match iterable {
            Type::Object(obj) => match obj.get(&t)? {
                Type::DefaultFunc(f) => {
                    return f.call(argv, env.clone(), self.operator.clone());
                }
                _ => {
                    crate::error("TypeError", "Unexpected operator: `[]`", self.operator.line);
                    return Err(Error::FromStr("TypeError".into()));
                }
            },
            _ => {
                crate::error(
                    "TypeError",
                    &format!("`{}` Cannot Indexed", iterable.type_()),
                    self.operator.line,
                );
                return Err(Error::FromStr("TypeError".into()));
            }
        }
    }
}

pub struct IndexAssignment {
    lhs: Box<dyn Eval>,
    rhs: Box<dyn Eval>,
    index: Box<dyn Eval>,
    equals: Token,
}

impl IndexAssignment {
    pub fn new(
        lhs: Box<dyn Eval>,
        index: Box<dyn Eval>,
        equals: Token,
        rhs: Box<dyn Eval>,
    ) -> Self {
        IndexAssignment {
            lhs,
            rhs,
            index,
            equals,
        }
    }
}

impl Eval for IndexAssignment {
    fn get_type(&self) -> String {
        String::from("IndexAssignment")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        let argv: Vec<Type> = vec![self.index.eval(env.clone())?, self.rhs.eval(env.clone())?];
        let t = Token::new(
            "set".to_string(),
            Type::Null,
            TokenType::Null,
            self.equals.line,
        );
        let iterable = self.lhs.eval(env.clone())?;
        match iterable {
            Type::Object(obj) => match obj.get(&t)? {
                Type::DefaultFunc(f) => {
                    return f.call(argv, env.clone(), self.equals.clone());
                }
                _ => {
                    crate::error("TypeError", "Unexpected operator: `[]`", self.equals.line);
                    return Err(Error::FromStr("TypeError".into()));
                }
            },
            _ => {
                crate::error(
                    "TypeError",
                    &format!("`{}` Cannot Indexed", iterable.type_()),
                    self.equals.line,
                );
                return Err(Error::FromStr("TypeError".into()));
            }
        }
    }
}

pub struct Super {
    superclass: Token,
}

impl Super {
    pub fn new(superclass: Token) -> Self {
        Super { superclass }
    }
}

impl Eval for Super {
    fn get_type(&self) -> String {
        String::from("Super")
    }

    fn eval(&self, env: Rc<RefCell<Environment>>) -> Result<Type, Error> {
        env.borrow().get(self.superclass.clone())
    }
}
