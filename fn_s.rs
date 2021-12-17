use crate::{
    fn_t::{Token, TokenType},
    fntype::types::Type,
};

pub struct Scanner {
    src: Vec<char>,
    tok: Vec<Token>,
    line: u64,
    start: u64,
    current: u64,
    multi_line_comment: u32,
}

impl Scanner {
    pub fn cell(&self) -> char {
        self.src[(self.current) as usize]
    }

    pub fn is_eof(&self) -> bool {
        self.current >= self.src.len() as u64
    }

    pub fn next(&self, next_char: char) -> bool {
        if self.is_eof() {
            false
        } else {
            self.src[(self.current + 1) as usize] == next_char
        }
    }

    pub fn get_identifier(&mut self) -> Token {
        while !self.is_eof()
            && (self.cell().is_alphabetic() || self.cell().is_digit(10) || self.cell() == '_')
        {
            self.current += 1;
        }

        self.current -= 1;

        let start = (self.start) as usize;
        let end = (self.current + 1) as usize;
        let identifier: String = self.src[start..end].iter().collect();

        match identifier.as_str() {
            "while" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::While,
                self.line,
            ),
            "and" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::And,
                self.line,
            ),
            "or" => Token::new(identifier.to_string(), Type::Null, TokenType::Or, self.line),
            "class" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Class,
                self.line,
            ),
            "func" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Fun,
                self.line,
            ),
            "for" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::For,
                self.line,
            ),
            "in" => Token::new(identifier.to_string(), Type::Null, TokenType::In, self.line),
            "if" => Token::new(identifier.to_string(), Type::Null, TokenType::If, self.line),
            "else" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Else,
                self.line,
            ),
            "return" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Return,
                self.line,
            ),
            "true" => Token::new(
                identifier.to_string(),
                Type::Bool(true),
                TokenType::True,
                self.line,
            ),
            "false" => Token::new(
                identifier.to_string(),
                Type::Bool(false),
                TokenType::False,
                self.line,
            ),
            "self" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::SelF,
                self.line,
            ),
            "let" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Let,
                self.line,
            ),
            "super" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Super,
                self.line,
            ),
            "null" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Null,
                self.line,
            ),
            "xor" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Xor,
                self.line,
            ),
            "break" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Break,
                self.line,
            ),
            "continue" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Continue,
                self.line,
            ),
            "using" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Using,
                self.line,
            ),
            "as" => Token::new(identifier.to_string(), Type::Null, TokenType::As, self.line),
            "from" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::From,
                self.line,
            ),
            "Write" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Print,
                self.line,
            ),
            "Writeln" => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Println,
                self.line,
            ),
            _ => Token::new(
                identifier.to_string(),
                Type::Null,
                TokenType::Ident,
                self.line,
            ),
        }
    }

    pub fn get_number(&mut self) -> Option<f64> {
        while !self.is_eof() && (self.cell().is_digit(10) || self.cell() == '.') {
            self.current += 1;
        }

        self.current -= 1;

        let start = (self.start) as usize;
        let end = (self.current + 1) as usize;
        let s: String = self.src[start..end].iter().collect();

        let n = s.trim_end().parse();
        if n.is_err() || s.trim_end().ends_with('.') {
            crate::error("SyntaxError", &format!("Invalid numeral {}", s), self.line);
            None
        } else {
            Some(n.unwrap())
        }
    }

    pub fn get_string(&mut self) -> Option<String> {
        while !self.next('"') && !self.is_eof() {
            if self.cell() == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }

        if self.is_eof() {
            crate::error(
                "SyntaxError",
                "EOL while scanning string literal",
                self.line,
            );
            return None;
        }

        self.current += 1;

        let start = (self.start + 1) as usize;
        let end = (self.current) as usize;
        let string: String = self.src[start..end].iter().collect();
        Some(string)
    }

    pub fn scan_token(&mut self) {
        while self.multi_line_comment > 0 {
            if self.cell() == '*' && self.next('/') {
                self.multi_line_comment -= 1;
                self.current += 1;
            } else if self.cell() == '/' && self.next('*') {
                self.multi_line_comment += 1;
                self.current += 1;
            } else if self.cell() == '\n' {
                self.line += 1;
            }
            self.current += 1;
        }

        if self.is_eof() {
            return;
        }

        let c = self.cell();

        match c {
            '(' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::LeftParen,
                self.line,
            )),
            ')' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::RightParen,
                self.line,
            )),
            '{' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::LeftBrace,
                self.line,
            )),
            '}' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::RightBrace,
                self.line,
            )),
            ',' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Comma,
                self.line,
            )),
            '.' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Dot,
                self.line,
            )),
            '-' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Minus,
                self.line,
            )),
            '+' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Plus,
                self.line,
            )),
            ';' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Semi,
                self.line,
            )),
            '*' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Star,
                self.line,
            )),
            '%' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Percent,
                self.line,
            )),
            '[' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Bra,
                self.line,
            )),
            ']' => self.tok.push(Token::new(
                c.to_string(),
                Type::Null,
                TokenType::Ket,
                self.line,
            )),

            '!' => self.tok.push(if self.next('=') {
                self.current += 1;
                Token::new("!=".to_string(), Type::Null, TokenType::BangEq, self.line)
            } else {
                Token::new(c.to_string(), Type::Null, TokenType::Bang, self.line)
            }),
            '<' => self.tok.push(if self.next('=') {
                self.current += 1;
                Token::new("<=".to_string(), Type::Null, TokenType::LessEq, self.line)
            } else {
                Token::new(c.to_string(), Type::Null, TokenType::Less, self.line)
            }),
            '>' => self.tok.push(if self.next('=') {
                self.current += 1;
                Token::new(
                    ">=".to_string(),
                    Type::Null,
                    TokenType::GreaterEq,
                    self.line,
                )
            } else {
                Token::new(c.to_string(), Type::Null, TokenType::Greater, self.line)
            }),
            '=' => self.tok.push(if self.next('=') {
                self.current += 1;
                Token::new("==".to_string(), Type::Null, TokenType::EqEq, self.line)
            } else {
                Token::new(c.to_string(), Type::Null, TokenType::Eq, self.line)
            }),

            '/' => {
                if self.next('/') {
                    while !(self.is_eof() || self.cell() == '\n') {
                        self.current += 1;
                    }
                    self.line += 1;
                } else if self.next('*') {
                    self.multi_line_comment += 1;
                    self.current += 1;
                } else {
                    self.tok.push(Token::new(
                        c.to_string(),
                        Type::Null,
                        TokenType::Slash,
                        self.line,
                    ));
                }
            }

            '"' => {
                if let Some(s) = self.get_string() {
                    self.tok.push(Token::new(
                        s.clone(),
                        Type::String(s.clone()),
                        TokenType::Str,
                        self.line,
                    ));
                }
            }
            '\n' => self.line += 1,
            '\r' | ' ' | '\t' => {}

            _ => {
                if c.is_digit(10) {
                    if let Some(n) = self.get_number() {
                        self.tok.push(Token::new(
                            n.to_string(),
                            Type::Number(n),
                            TokenType::Num,
                            self.line,
                        ));
                    }
                } else if c.is_alphabetic() {
                    let t = self.get_identifier();
                    self.tok.push(t);
                } else {
                    crate::error(
                        "SyntaxError",
                        &format!("Unexpected character {}", c),
                        self.line,
                    );
                }
            }
        }
        self.current += 1;
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_eof() {
            self.start = self.current;
            self.scan_token();
        }
        self.tok.clone()
    }

    pub fn new(source: String) -> Scanner {
        Scanner {
            src: source.chars().collect(),
            tok: Vec::new(),
            line: 1,
            start: 0,
            current: 0,
            multi_line_comment: 0,
        }
    }
}
