use crate::fn_a;
use crate::fn_a::Eval;
use crate::fn_t::{Token, TokenType};
use crate::fntype::func::Function;
use crate::fntype::types::Type;

pub struct Parser {
    tokens: Vec<Token>,
    current: u32,
    loop_counter: u32,
    function_counter: u32,
    class_counter: u32,
    superclass: Vec<Option<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens: tokens,
            current: 0,
            loop_counter: 0,
            function_counter: 0,
            class_counter: 0,
            superclass: vec![],
        }
    }
    fn at_end(&self) -> bool {
        (self.current as usize) >= self.tokens.len()
    }

    fn peek(&self) -> Token {
        self.tokens[self.current as usize].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[(self.current - 1) as usize].clone()
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check_type(&self, t: &TokenType) -> bool {
        if self.at_end() {
            return false;
        }
        self.peek().t == *t
    }

    pub fn parse(&mut self) -> Result<Vec<Box<dyn Eval>>, String> {
        let mut stmt_vec: Vec<Box<dyn Eval>> = Vec::new();
        let mut error = false;

        while !self.at_end() {
            match self.declaration() {
                Ok(stmt) => stmt_vec.push(stmt),
                Err(_) => error = true,
            }
        }
        if error {
            return Err("ParseError".into());
        }
        Ok(stmt_vec)
    }

    fn declaration(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.peek().t {
            TokenType::Using | TokenType::From => self.import_statement(),
            TokenType::Let => self.var_declaration(),
            TokenType::Fun => match self.function_declaration() {
                Err(e) => Err(e),
                Ok(f) => Ok(Box::new(f)),
            },
            TokenType::Class => self.class_declaration(),
            _ => self.statement(),
        }
    }

    fn import_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        let mut alias: Option<Token> = None;
        if self.check_type(&TokenType::From) {
            self.advance();
            let module = self.consume(TokenType::Ident, "Expect identifier after `from`")?;
            self.consume(TokenType::Using, "Expect `import`")?;
            let item = self.consume(TokenType::Ident, "Expect identifier after `import`")?;
            if self.check_type(&TokenType::As) {
                self.advance();
                alias = Some(self.consume(TokenType::Ident, "Expect identifier after `as`")?);
            }
            self.consume(TokenType::Semi, "Expect ';' after statement")?;
            return Ok(Box::new(fn_a::Import::new(module, Some(item), alias)));
        }
        self.consume(TokenType::Using, "Expect `import`")?;
        let module = self.consume(TokenType::Ident, "Expect identifier after `import`")?;

        if self.check_type(&TokenType::As) {
            self.advance();
            alias = Some(self.consume(TokenType::Ident, "Expect identifier after `as`")?);
        }
        self.consume(TokenType::Semi, "Expect ';' after statement")?;
        Ok(Box::new(fn_a::Import::new(module, None, alias)))
    }

    fn class_declaration(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();
        self.class_counter += 1;
        let identifier = self.consume(
            TokenType::Ident,
            "Expect identifier after class declaration",
        )?;
        let mut superclass: Option<Token> = None;
        if self.check_type(&TokenType::Less) {
            self.advance();
            let s = self.consume(TokenType::Ident, "Expect superclass identifier after `<`")?;
            if s.lexeme == identifier.lexeme {
                crate::error("NameError", "cannot inherit from self", identifier.line);
                return Err("NameError".to_string());
            }
            superclass = Some(s);
            self.superclass.push(superclass.clone());
        } else {
            self.superclass.push(None);
        }
        self.consume(TokenType::LeftBrace, "Expect '{' before class body")?;

        let mut methods: Vec<Function> = Vec::new();
        while !self.check_type(&TokenType::RightBrace) {
            methods.push((self.function_declaration()?).f.clone());
        }
        self.consume(TokenType::RightBrace, "Expect '}' after class body")?;
        self.class_counter -= 1;
        self.superclass.pop();
        Ok(Box::new(fn_a::ClassDeclr::new(
            identifier, superclass, methods,
        )))
    }

    fn function_declaration(&mut self) -> Result<fn_a::FunDeclaration, String> {
        self.consume(TokenType::Fun, "Expect 'fun' before function declaration")?;
        self.function_counter += 1;
        let identifier = self.consume(
            TokenType::Ident,
            "Expect identifier after function declaration",
        )?;
        self.consume(
            TokenType::LeftParen,
            "Expect '(' after function declaration",
        )?;

        let mut arguments: Vec<Token> = Vec::new();

        while !self.check_type(&TokenType::RightParen) {
            if arguments.len() >= (255 as usize) {
                crate::error(
                    "ParsingError",
                    "Max argument len reached (255)",
                    self.peek().line,
                );
                return Err("Max argument len reached (255)".into());
            }
            arguments.push(self.advance());
            while self.check_type(&TokenType::Comma) {
                self.advance();
                arguments.push(self.advance());
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments")?;
        let fbody = self.block_statement()?;
        self.function_counter -= 1;

        Ok(fn_a::FunDeclaration::new(identifier, arguments, fbody))
    }

    fn var_declaration(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();
        let identifier = self.consume(TokenType::Ident, "Expect identifier")?;

        let mut value: Box<dyn Eval> = Box::new(
            fn_a::Literal::new(Token::new(
                "nil".to_string(),
                Type::Null,
                TokenType::Null,
                self.previous().line,
            ))
            .unwrap(),
        );

        if self.check_type(&TokenType::Eq) {
            self.advance();
            let expr = self.expression();
            if expr.is_err() {
                crate::error(
                    "SyntaxError",
                    "Invalid variable declaration",
                    self.previous().line,
                );
                return Err("Invalid variable declaration".into());
            } else {
                value = expr.unwrap();
            }
        }

        self.consume(TokenType::Semi, "Expect ';' after expression")?;
        Ok(Box::new(fn_a::VarDeclaration::new(identifier, value)))
    }

    fn statement(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.peek().t {
            TokenType::If => self.if_statement(),
            TokenType::Print => self.print_statement(),
            TokenType::Println => self.println_statement(),
            TokenType::Return => self.return_statement(),
            TokenType::While => self.while_loop(),
            TokenType::For => self.for_loop(),
            TokenType::LeftBrace => match self.block_statement() {
                Ok(block) => Ok(Box::new(block)),
                Err(e) => Err(e),
            },
            _ => self.expression_stmt(),
        }
    }

    fn for_loop(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();
        let alias = self.consume(TokenType::Ident, "Expect identifier after `for`")?;
        self.consume(TokenType::In, "Expect `in` after `for` statement")?;
        let iterable = self.brackets()?;

        self.loop_counter += 1;
        let stmt: fn_a::BlockStmt;
        if self.check_type(&TokenType::LeftBrace) {
            match self.block_statement() {
                Ok(x) => stmt = x,
                Err(e) => {
                    self.synchronize();
                    return Err(e);
                }
            }
        } else {
            crate::error(
                "SyntaxError",
                "Expected '{' after 'for' statement",
                self.previous().line,
            );
            self.synchronize();
            return Err("Expected '{' after 'for' statement".into());
        }
        self.loop_counter -= 1;
        Ok(Box::new(fn_a::ForLoop::new(alias, iterable, stmt)))
    }

    fn while_loop(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();

        let cond = self.expression();

        self.loop_counter += 1;
        let stmt: fn_a::BlockStmt;
        if self.check_type(&TokenType::LeftBrace) {
            match self.block_statement() {
                Ok(x) => stmt = x,
                Err(e) => {
                    self.synchronize();
                    return Err(e);
                }
            }
        } else {
            crate::error(
                "SyntaxError",
                "Expected '{' after 'while' condition",
                self.previous().line,
            );
            self.synchronize();
            return Err("Expected '{' after 'while' condition".into());
        }
        self.loop_counter -= 1;
        match cond {
            Ok(x) => Ok(Box::new(fn_a::WhileLoop::new(x, stmt))),
            _ => {
                crate::error(
                    "SyntaxError",
                    "Invalid condition for while loop",
                    self.peek().line,
                );
                self.synchronize();
                Err("Invalid condition for while loop".into())
            }
        }
    }

    fn if_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();

        let cond = self.expression();

        let stmt: Box<dyn Eval>;
        if self.check_type(&TokenType::LeftBrace) {
            match self.statement() {
                Ok(x) => stmt = x,
                _ => {
                    return Err("invalid statement in 'if' block".into());
                }
            }
        } else {
            crate::error(
                "SyntaxError",
                "Expected '{' after 'if' condition",
                self.previous().line,
            );
            return Err("Expected '{' after 'if' condition".into());
        }

        let mut else_stmt: Option<Box<dyn Eval>> = None;
        if self.check_type(&TokenType::Else) {
            self.advance();
            if self.check_type(&TokenType::LeftBrace) {
                match self.statement() {
                    Ok(x) => else_stmt = Some(x),
                    _ => {
                        return Err("invalid statement in 'else' block".into());
                    }
                }
            } else {
                crate::error(
                    "SyntaxError",
                    "Expected '{' after 'else' keyword",
                    self.previous().line,
                );
                return Err("Expected '{' after 'else' keyword".into());
            }
        }

        if cond.is_err() {
            crate::error("ParserError", "invalid if statement", self.peek().line);
            return Err("invalid if statement".into());
        }
        Ok(Box::new(fn_a::IfStmt::new(cond.unwrap(), stmt, else_stmt)))
    }

    fn block_statement(&mut self) -> Result<fn_a::BlockStmt, String> {
        self.advance();

        let mut statements: Vec<Box<dyn Eval>> = Vec::new();

        while !(self.at_end() || self.check_type(&TokenType::RightBrace)) {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(e) => return Err(e),
            }
        }

        match self.consume(TokenType::RightBrace, "Expect '}' after block") {
            Ok(_) => Ok(fn_a::BlockStmt::new(statements)),
            Err(e) => Err(e),
        }
    }

    fn print_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();
        let expr = self.expression()?;
        match self.consume(TokenType::Semi, "Expect ';' after expression") {
            Ok(_) => Ok(Box::new(fn_a::PrintStmt::new(expr))),
            Err(e) => Err(e),
        }
    }

    fn println_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();
        let expr = self.expression()?;
        match self.consume(TokenType::Semi, "Expect ';' after expression") {
            Ok(_) => Ok(Box::new(fn_a::PrintLnStmt::new(expr))),
            Err(e) => Err(e),
        }
    }

    fn return_statement(&mut self) -> Result<Box<dyn Eval>, String> {
        self.advance();
        if self.function_counter < 1 {
            crate::error(
                "SyntaxError",
                "return statement outside of function statement",
                self.peek().line,
            );
            self.synchronize();
            return Err("return statement outside of function".into());
        }

        let mut value: Box<dyn Eval> = Box::new(
            fn_a::Literal::new(Token::new(
                "nil".to_string(),
                Type::Null,
                TokenType::Null,
                self.previous().line,
            ))
            .unwrap(),
        );
        if !self.check_type(&TokenType::Semi) {
            value = self.expression()?;
        }
        self.consume(TokenType::Semi, "Expect ';' after expression")?;
        Ok(Box::new(fn_a::Return::new(value)))
    }

    fn expression_stmt(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.expression() {
            Err(e) => Err(e),
            Ok(expr) => {
                if self.check_type(&TokenType::Semi) {
                    self.advance();
                }
                Ok(Box::new(fn_a::ExprStmt::new(expr)))
            }
        }
    }

    pub fn expression(&mut self) -> Result<Box<dyn Eval>, String> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Box<dyn Eval>, String> {
        let t = self.peek();
        let ret_index = self.current;

        let expr = self.logical_or()?;
        if self.check_type(&TokenType::Eq) {
            let equals = self.advance();

            if expr.get_type().contains("Variable") {
                match self.assignment() {
                    Err(_) => {
                        crate::error("SyntaxError", "invalid rhs for assignment", equals.line);
                        return Err("invalid rhs for assignment".into());
                    }
                    Ok(rhs) => {
                        return Ok(Box::new(fn_a::Assignment::new(t, rhs)));
                    }
                }
            } else if expr.get_type() == String::from("Getter") {
                self.current = ret_index;
                let expr = self.primary()?;
                self.advance();
                let name = self.consume(TokenType::Ident, "Expect identifier after '.'")?;
                self.advance();
                match self.assignment() {
                    Err(_) => {
                        crate::error("SyntaxError", "invalid rhs for assignment", equals.line);
                        return Err("invalid rhs for assignment".into());
                    }
                    Ok(rhs) => {
                        return Ok(Box::new(fn_a::Set::new(name, expr, rhs)));
                    }
                }
            } else if expr.get_type() == String::from("Index") {
                self.current = ret_index;
                match self.primary() {
                    Ok(mut expr) => {
                        while self.check_type(&TokenType::Bra) {
                            let operator = self.advance();
                            expr = match self.term() {
                                Ok(index) => {
                                    if self.tokens[(self.current + 1) as usize].t == TokenType::Eq {
                                        self.consume(TokenType::Ket, "Expect ']'")?;
                                        let equals = self.consume(TokenType::Eq, "Expect `=`")?;
                                        match self.assignment() {
                                            Err(_) => {
                                                crate::error(
                                                    "SyntaxError",
                                                    "invalid rhs for assignment",
                                                    equals.line,
                                                );
                                                return Err("invalid rhs for assignment".into());
                                            }
                                            Ok(rhs) => {
                                                return Ok(Box::new(fn_a::IndexAssignment::new(
                                                    expr, index, equals, rhs,
                                                )));
                                            }
                                        }
                                    } else {
                                        let ret = Box::new(fn_a::Index::new(
                                            expr,
                                            operator.clone(),
                                            index,
                                        ));
                                        self.consume(TokenType::Ket, "Expect ']'")?;
                                        ret
                                    }
                                }
                                Err(e) => return Err(e),
                            };
                        }
                        Ok(expr)
                    }
                    Err(e) => Err(e),
                }
            } else {
                crate::error(
                    "SyntaxError",
                    "invalid target variable for assignment",
                    equals.line,
                );
                return Err("invalid target variable for assignment".into());
            }
        } else {
            return Ok(expr);
        }
    }

    fn logical_or(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.logical_and() {
            Ok(mut expr) => {
                while self.check_type(&TokenType::Or) || self.check_type(&TokenType::Xor) {
                    let operator = self.advance();
                    expr = match self.logical_and() {
                        Ok(right) => {
                            Box::new(fn_a::LogicalExpr::new(operator, expr, right).unwrap())
                        }
                        Err(e) => return Err(e),
                    };
                }
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn logical_and(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.equality() {
            Ok(mut expr) => {
                while self.check_type(&TokenType::And) {
                    let operator = self.advance();
                    expr = match self.equality() {
                        Ok(right) => {
                            Box::new(fn_a::LogicalExpr::new(operator, expr, right).unwrap())
                        }
                        Err(e) => return Err(e),
                    };
                }
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn equality(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.comparison() {
            Ok(mut expr) => {
                while [TokenType::BangEq, TokenType::EqEq]
                    .iter()
                    .any(|x| self.check_type(x))
                {
                    let operator = self.advance();
                    expr = match self.comparison() {
                        Ok(right) => Box::new(fn_a::Binary::new(operator, expr, right).unwrap()),
                        Err(e) => return Err(e),
                    };
                }
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn comparison(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.term() {
            Ok(mut expr) => {
                while [
                    TokenType::Greater,
                    TokenType::GreaterEq,
                    TokenType::Less,
                    TokenType::LessEq,
                ]
                .iter()
                .any(|x| self.check_type(x))
                {
                    let operator = self.advance();
                    expr = match self.term() {
                        Ok(right) => Box::new(fn_a::Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn term(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.factor() {
            Ok(mut expr) => {
                while [TokenType::Plus, TokenType::Minus]
                    .iter()
                    .any(|x| self.check_type(x))
                {
                    let operator = self.advance();
                    expr = match self.factor() {
                        Ok(right) => Box::new(fn_a::Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn factor(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.unary() {
            Ok(mut expr) => {
                while [TokenType::Star, TokenType::Slash, TokenType::Percent]
                    .iter()
                    .any(|x| self.check_type(x))
                {
                    let operator = self.advance();
                    expr = match self.unary() {
                        Ok(right) => Box::new(fn_a::Binary::new(operator, expr, right).unwrap()),
                        Err(right) => return Err(right),
                    };
                }
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn unary(&mut self) -> Result<Box<dyn Eval>, String> {
        if [TokenType::Bang, TokenType::Minus]
            .iter()
            .any(|x| self.check_type(x))
        {
            let operator = self.advance();
            match self.unary() {
                Ok(right) => return Ok(Box::new(fn_a::Unary::new(operator, right).unwrap())),
                Err(right) => return Err(right),
            }
        }
        match self.function_call() {
            Err(e) => {
                self.synchronize();
                Err(e)
            }
            Ok(expr) => Ok(expr),
        }
    }

    fn brackets(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.function_call() {
            Ok(mut expr) => {
                while self.check_type(&TokenType::Bra) {
                    let operator = self.advance();
                    expr = match self.term() {
                        Ok(index) => {
                            let ret = Box::new(fn_a::Index::new(expr, operator.clone(), index));
                            self.consume(TokenType::Ket, "Expect ']'")?;
                            ret
                        }
                        Err(e) => return Err(e),
                    };
                }
                Ok(expr)
            }
            Err(e) => Err(e),
        }
    }

    fn function_call(&mut self) -> Result<Box<dyn Eval>, String> {
        match self.primary() {
            Ok(mut expr) => loop {
                if self.check_type(&TokenType::LeftParen) {
                    self.advance();

                    let mut arguments: Vec<Box<dyn Eval>> = Vec::new();

                    if !self.check_type(&TokenType::RightParen) {
                        if arguments.len() >= (255 as usize) {
                            crate::error(
                                "ParsingError",
                                "Max argument len reached (255)",
                                self.peek().line,
                            );
                            return Err("Max argument len reached (255)".into());
                        }
                        arguments.push(self.expression()?);
                        while self.check_type(&TokenType::Comma) {
                            self.advance();
                            arguments.push(self.expression()?);
                        }
                    }
                    let paren =
                        self.consume(TokenType::RightParen, "Expect ')' after arguments")?;
                    expr = Box::new(fn_a::Call::new(expr, paren, arguments));
                } else if self.check_type(&TokenType::Dot) {
                    self.advance();
                    let name = self.consume(TokenType::Ident, "Expect identifier after '.'")?;
                    expr = Box::new(fn_a::Get::new(name, expr));
                } else if self.check_type(&TokenType::Bra) {
                    while self.check_type(&TokenType::Bra) {
                        let operator = self.advance();
                        expr = match self.term() {
                            Ok(index) => {
                                let ret = Box::new(fn_a::Index::new(expr, operator.clone(), index));
                                self.consume(TokenType::Ket, "Expect ']'")?;
                                ret
                            }
                            Err(e) => return Err(e),
                        };
                    }
                } else {
                    return Ok(expr);
                }
            },
            Err(e) => Err(e),
        }
    }

    fn primary(&mut self) -> Result<Box<dyn Eval>, String> {
        if vec![
            TokenType::Num,
            TokenType::Str,
            TokenType::Null,
            TokenType::True,
            TokenType::False,
        ]
        .iter()
        .any(|x| self.check_type(x))
        {
            return Ok(Box::new(fn_a::Literal::new(self.advance()).unwrap()));
        }

        if self.check_type(&TokenType::Ident) {
            return Ok(Box::new(fn_a::Variable::new(self.advance())));
        }

        if self.check_type(&TokenType::SelF) {
            if self.class_counter == 0 {
                crate::error(
                    "SyntaxError",
                    "`self` outside class declaration",
                    self.peek().line,
                );
                return Err("`self` outside class declaration".into());
            }
            return Ok(Box::new(fn_a::Variable::new(self.advance())));
        }

        if self.check_type(&TokenType::Super) {
            if self.class_counter == 0 {
                crate::error(
                    "SyntaxError",
                    "`super` outside class declaration",
                    self.peek().line,
                );
                return Err("`super` outside class declaration".into());
            }
            self.advance();
            match self.superclass.last() {
                Some(x) => match x {
                    Some(s) => return Ok(Box::new(fn_a::Super::new(s.clone()))),
                    None => {
                        crate::error("AttributeError", "illegal `super`", self.peek().line);
                        return Err("illegal `super`".into());
                    }
                },
                None => {
                    crate::error("AttributeError", "illegal `super`", self.peek().line);
                    return Err("illegal `super`".into());
                }
            }
        }

        if self.check_type(&TokenType::LeftBrace) {
            match self.block_statement() {
                Ok(block) => return Ok(Box::new(block)),
                Err(_) => return Err("Invalid Expression".into()),
            }
        }

        if self.check_type(&TokenType::LeftParen) {
            self.advance();
            match self.expression() {
                Ok(expr) => {
                    match self.consume(TokenType::RightParen, "Expected ')' after expression") {
                        Err(e) => return Err(e),
                        _ => return Ok(Box::new(fn_a::Grouping::new(expr))),
                    };
                }
                Err(e) => return Err(e),
            }
        }

        if self.check_type(&TokenType::Break) {
            self.advance();
            match self.consume(TokenType::Semi, "Expected ';' after statement") {
                Err(e) => return Err(e),
                _ => {
                    if self.loop_counter == 0 {
                        crate::error("SyntaxError", "'break' outside loop", self.previous().line);
                        return Err("'break' outside loop".into());
                    }
                    return Ok(Box::new(fn_a::Break::new()));
                }
            };
        }

        if self.check_type(&TokenType::Continue) {
            self.advance();
            match self.consume(TokenType::Semi, "Expected ';' after statement") {
                Err(e) => return Err(e),
                _ => {
                    if self.loop_counter == 0 {
                        crate::error(
                            "SyntaxError",
                            "'continue' outside loop",
                            self.previous().line,
                        );
                        return Err("'continue' outside loop".into());
                    }
                    return Ok(Box::new(fn_a::Continue::new()));
                }
            };
        }

        crate::error(
            "SyntaxError",
            &format!("expected expression, got {}", self.peek()),
            self.peek().line,
        );
        Err(format!("Expected expression, got {}", self.peek()).into())
    }

    fn consume(&mut self, t: TokenType, message: &str) -> Result<Token, String> {
        if self.check_type(&t) {
            Ok(self.advance())
        } else {
            crate::error("SyntaxError", message, self.previous().line);
            Err(message.into())
        }
    }

    fn synchronize(&mut self) {
        while !self.at_end() {
            match self.peek().t {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Let
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Println
                | TokenType::Return => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }
}
