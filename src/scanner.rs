use crate::keywords::KEYWORD_TOKENS;
use crate::error::Error;

#[derive(Debug, Copy, Clone)]
pub struct Token<'lex> {
    pub token_type: TokenType,
    pub lexeme: &'lex str,
    pub line: usize,
}

impl Token<'_> {
    fn new(token_type: TokenType, lexeme: &str, line: usize) -> Token<'_> {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus,
    Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier, String, Number,

    // Keywords.
    And, Class, Else, False,
    For, Fun, If, Nil, Or,
    Print, Return, Super, This,
    True, Var, While,

    EOF,
}

pub struct Scanner<'src> {
    source: &'src str,
    chars: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Scanner<'src> {
        Scanner {
            source,
            chars: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'src>, Error> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return Ok(self.make_token(TokenType::EOF));
        }

        let c = self.advance();

        let token = match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                let token_type = if self.match_char('=') { TokenType::BangEqual } else { TokenType::Bang };
                self.make_token(token_type)
            },
            '=' => {
                let token_type = if self.match_char('=') { TokenType::EqualEqual } else { TokenType::Equal };
                self.make_token(token_type)
            },
            '<' => {
                let token_type = if self.match_char('=') { TokenType::LessEqual } else { TokenType::Less };
                self.make_token(token_type)
            },
            '>' => {
                let token_type = if self.match_char('=') { TokenType::GreaterEqual } else { TokenType::Greater };
                self.make_token(token_type)
            },
            '"' => self.string()?,
            c if c.is_digit(10) => self.number(),
            c if c.is_alphabetic() || c == '_' => self.identifier(),
            _ => return Err(Error {
                message: format!("Unsupported character '{}'.", c),
                line: self.line,
            }),
        };

        Ok(token)
    }
}

impl<'src> Scanner<'src> {
    fn string(&mut self) -> Result<Token<'src>, Error> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' { self.line += 1; }
            self.advance();
        }

        // Unterminated string found.
        if self.is_at_end() {
            return Err(Error {
                message: "Unterminated string found.".to_string(),
                line: self.line,
            });
        }

        // Consume closing '"'.
        self.advance();

        Ok(self.make_token(TokenType::String))
    }

    fn number(&mut self) -> Token<'src> {
        while self.peek().is_digit(10) { self.advance(); }

        // Check if has fractional part.
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            // Consume the dot.
            self.advance();
            while self.peek().is_digit(10) { self.advance(); }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token<'src> {
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }

        // Check if identifier is reserved.
        let lexeme_slice = &self.source[self.start..self.current];
        let token_type = match KEYWORD_TOKENS.get(lexeme_slice) {
            Some(v) => *v,
            None => TokenType::Identifier,
        };
        self.make_token(token_type)
    }

    fn make_token(&mut self, token_type: TokenType) -> Token<'src> {
        let token = Token::new(token_type, &self.source[self.start..self.current], self.line);
        self.start = self.current;
        token
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.chars[self.current - 1]
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.chars[self.current] != expected {
             false
        } else {
            self.current += 1;
            true
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();

            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                },
                '\n' => {
                    self.line += 1;
                    self.advance();
                },
                '/' if self.peek_next() == '/' => {
                    while !self.is_at_end() && self.peek() != '\n' {
                        self.advance();
                    }
                },
                _ => break,
            }
        }
    }
}
