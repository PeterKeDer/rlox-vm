use std::mem;
use std::convert::TryFrom;

use num_enum::TryFromPrimitive;

use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, OpCode};
use crate::object::{ObjectPtr, Object};
use crate::error::{Error, Result};

/// Parsing precedence, from lowest to highest.
#[derive(Debug, TryFromPrimitive, Copy, Clone, Ord, PartialOrd, Eq, PartialEq)]
#[repr(u8)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    /// Get the `Precedence` that is one higher than the current one.
    /// Precondition: `self` is not `Precedence::Primary`.
    fn higher(self) -> Precedence {
        Precedence::try_from(self as u8).expect("Cannot get higher precedence for Primary.")
    }
}

macro_rules! parse_rules {
    ( $( $token_type:ident => ($prefix:ident, $infix:ident, $precedence:ident), )* ) => {
        impl Compiler<'_> {
            /// Get the corresponding infix `Precedence` for `token_type`.
            fn get_precedence(&self, token_type: TokenType) -> Precedence {
                match token_type {
                    $(
                        TokenType::$token_type => Precedence::$precedence,
                    )*
                }
            }

            /// Parse the prefix expression for `token_type`.
            fn parse_prefix(&mut self, token_type: TokenType) -> Result<()> {
                match token_type {
                    $(
                        TokenType::$token_type => self.$prefix(),
                    )*
                }
            }

            /// Parse the infix expression for `token_type`.
            fn parse_infix(&mut self, token_type: TokenType) -> Result<()> {
                match token_type {
                    $(
                        TokenType::$token_type => self.$infix(),
                    )*
                }
            }
        }
    };
}

// Implement function `get_precedence`, `parse_prefix`, and `parse_infix` for `Compiler`.
// Format: `token_type` => (`prefix`, `infix`, `precedence`).
// `prefix` and `infix` correspond to member functions belonging to `Compiler`.
parse_rules! {
//  Token Type          Prefix      Infix       Precedence
    LeftParen       => (grouping,   none,       None        ),
    RightParen      => (none,       none,       None        ),
    LeftBrace       => (none,       none,       None        ),
    RightBrace      => (none,       none,       None        ),
    Comma           => (none,       none,       None        ),
    Dot             => (none,       none,       None        ),
    Minus           => (unary,      binary,     Term        ),
    Plus            => (none,       binary,     Term        ),
    Semicolon       => (none,       none,       None        ),
    Slash           => (none,       binary,     Factor      ),
    Star            => (none,       binary,     Factor      ),
    Bang            => (unary,      none,       None        ),
    BangEqual       => (none,       binary,     Equality    ),
    Equal           => (none,       none,       None        ),
    EqualEqual      => (none,       binary,     Equality    ),
    Greater         => (none,       binary,     Comparison  ),
    GreaterEqual    => (none,       binary,     Comparison  ),
    Less            => (none,       binary,     Comparison  ),
    LessEqual       => (none,       binary,     Comparison  ),
    Identifier      => (variable,   none,       None        ),
    String          => (string,     none,       None        ),
    Number          => (number,     none,       None        ),
    And             => (none,       binary,     And         ),
    Class           => (none,       none,       None        ),
    Else            => (none,       none,       None        ),
    False           => (literal,    none,       None        ),
    For             => (none,       none,       None        ),
    Fun             => (none,       none,       None        ),
    If              => (none,       none,       None        ),
    Nil             => (literal,    none,       None        ),
    Or              => (none,       binary,     Or          ),
    Print           => (none,       none,       None        ),
    Return          => (none,       none,       None        ),
    Super           => (none,       none,       None        ),
    This            => (none,       none,       None        ),
    True            => (literal,    none,       None        ),
    Var             => (none,       none,       None        ),
    While           => (none,       none,       None        ),
    EOF             => (none,       none,       None        ),
}

/// Match a `TokenType`. If any branch matched, `Compiler.advance` is called first.
macro_rules! match_advance {
    ( $c:expr, { $( $ty:ident => $expr:expr, )* $( _ => $or_else:expr, )? } ) => {
        match $c.current().token_type {
            $(
                TokenType::$ty => {
                    $c.advance()?;
                    $expr
                },
            )*
            $(
                _ => $or_else,
            )?
        }
    };
}

pub struct Compiler<'src> {
    scanner: Scanner<'src>,
    chunk: Chunk,
    current: Option<Token<'src>>,
    previous: Option<Token<'src>>,
    objects: Vec<ObjectPtr>,
    precedence: Precedence,
    has_error: bool,
}

impl Compiler<'_> {
    /// Compile `Token`s from `scanner` and return the `Chunk` produced and the `ObjectPtr`s on the heap.
    pub fn compile(scanner: Scanner<'_>) -> Result<(Chunk, Vec<ObjectPtr>)> {
        let mut compiler = Compiler {
            scanner,
            chunk: Chunk::new(),
            current: None,
            previous: None,
            objects: vec![],
            precedence: Precedence::None,
            has_error: false,
        };

        // This initializes `current` field of `compiler` by calling `next_token` on `scanner`
        compiler.advance()?;

        // Parses and compiles a program
        compiler.program()?;

        if compiler.has_error {
            return Err(Error::new("At least one error has occured.".to_string(), 1));
        }

        Ok((compiler.chunk, compiler.objects))
    }

    fn program(&mut self) -> Result<()> {
        while !self.match_token(TokenType::EOF)? {
            self.declaration()?;
        }

        Ok(())
    }

    fn declaration(&mut self) -> Result<()> {
        match self.statement() {
            Ok(_) => Ok(()),
            Err(err) => {
                // TODO: better error handling
                self.has_error = true;
                println!("[Error]: {:?}", err);
                self.synchronize()?;
                Ok(())
            },
        }
    }

    fn statement(&mut self) -> Result<()> {
        match_advance!(self, {
            Var => self.variable_declaration(),
            Print => self.print_statement(),
            _ => self.expression_statement(),
        })
    }

    fn variable_declaration(&mut self) -> Result<()> {
        let global = self.parse_variable("Expect variable name.".to_string())?;

        if self.match_token(TokenType::Equal)? {
            self.expression()?;
        } else {
            self.emit_byte(OpCode::Nil as u8);
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.".to_string())?;

        self.define_variable(global);

        Ok(())
    }

    fn print_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.".to_string())?;
        self.emit_byte(OpCode::Print as u8);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<()> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.".to_string())?;
        self.emit_byte(OpCode::Pop as u8);
        Ok(())
    }

    /// Parse anything with given precedence or higher.
    /// Precedence: `precedence` is not `Precedence::None`.
    fn parse_precedence(&mut self, precedence: Precedence) -> Result<()> {
        let prev_precedence = self.precedence;
        let result = self.parse_precedence_util(precedence);
        self.precedence = prev_precedence;
        result
    }

    fn parse_precedence_util(&mut self, precedence: Precedence) -> Result<()> {
        self.precedence = precedence;
        self.advance()?;
        self.parse_prefix(self.previous().token_type)?;

        loop {
            let token_type = self.current().token_type;
            let infix_precedence = self.get_precedence(token_type);
            self.precedence = infix_precedence;

            if precedence > infix_precedence {
                // Stop parsing since the next token has lower precedence, or is not an infix operator at all
                break;
            }

            // Otherwise, advance and run it
            self.advance()?;
            self.parse_infix(token_type)?;
        }

        Ok(())
    }

    fn expression(&mut self) -> Result<()> {
        self.parse_precedence(Precedence::Assignment)
    }

    /// Parse nothing and return error, facilitates `ParseRule` initialization.
    fn none(&mut self) -> Result<()> {
        let token = self.previous();
        Err(Error::new(format!("Expected expression, got {:?}.", token.token_type), token.line))
    }

    fn grouping(&mut self) -> Result<()> {
        // Assume that '(' is already consumed
        self.expression()?;
        self.consume(TokenType::RightParen, "Expected closing ')'.".to_string())?;
        Ok(())
    }

    fn unary(&mut self) -> Result<()> {
        let operator = self.previous().token_type;

        self.parse_precedence(Precedence::Unary)?;

        match operator {
            TokenType::Minus => self.emit_byte(OpCode::Negate as u8),
            TokenType::Bang => self.emit_byte(OpCode::Not as u8),
            _ => panic!("Unexpected unary operator {:?}.", operator),
        }

        Ok(())
    }

    fn binary(&mut self) -> Result<()> {
        // Note that this only parses the expression after the operator
        let operator = self.previous().token_type;

        self.parse_precedence(self.get_precedence(operator).higher())?;

        match operator {
            TokenType::Plus => self.emit_byte(OpCode::Add as u8),
            TokenType::Minus => self.emit_byte(OpCode::Subtract as u8),
            TokenType::Star => self.emit_byte(OpCode::Multiply as u8),
            TokenType::Slash => self.emit_byte(OpCode::Divide as u8),
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal as u8),
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::Less => self.emit_byte(OpCode::Less as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            TokenType::Greater => self.emit_byte(OpCode::Greater as u8),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::And => self.emit_byte(OpCode::And as u8),
            TokenType::Or => self.emit_byte(OpCode::Or as u8),
            _ => panic!("Unexpected binary operator {:?}.", operator),
        }

        Ok(())
    }

    fn variable(&mut self) -> Result<()> {
        self.named_variable(self.previous().lexeme.iter().collect())
    }

    fn string(&mut self) -> Result<()> {
        let token = self.previous();

        let string = token.lexeme[1..token.lexeme.len() - 1].iter().collect();
        let value = Object::String(string);

        self.emit_constant(value)?;
        Ok(())
    }

    fn number(&mut self) -> Result<()> {
        let token = self.previous();

        let value = token.lexeme.iter().collect::<String>()
            .parse()
            .map(Object::Number)
            .map_err(|_| Error::new("Cannot parse float.".to_string(), token.line))?;

        self.emit_constant(value)?;
        Ok(())
    }

    fn literal(&mut self) -> Result<()> {
        let byte = match self.previous().token_type {
            TokenType::True => OpCode::True,
            TokenType::False => OpCode::False,
            TokenType::Nil => OpCode::Nil,
            t => panic!("Unexpected literal {:?}.", t),
        } as u8;

        self.emit_byte(byte);
        Ok(())
    }

    /// Emit code to get or set a global variable with name.
    fn named_variable(&mut self, name: String) -> Result<()> {
        let constant = self.identifier_constant(name)?;

        if self.precedence <= Precedence::Assignment && self.match_token(TokenType::Equal)? {
            self.expression()?;
            self.emit_bytes(OpCode::SetGlobal as u8, constant);
        } else {
            self.emit_bytes(OpCode::GetGlobal as u8, constant);
        }

        Ok(())
    }

    /// Parse a variable and return the constant address to its name (String).
    fn parse_variable(&mut self, message: String) -> Result<u8> {
        self.consume(TokenType::Identifier, message)?;
        self.identifier_constant(self.previous().lexeme.iter().collect())
    }
}

impl<'src> Compiler<'src> {
    fn synchronize(&mut self) -> Result<()> {
        while self.current().token_type != TokenType::EOF {
            if self.previous().token_type == TokenType::Semicolon {
                return Ok(());
            }

            match self.current().token_type {
                TokenType::Class |
                TokenType::Fun |
                TokenType::Var |
                TokenType::For |
                TokenType::If |
                TokenType::While |
                TokenType::Print |
                TokenType::Return => return Ok(()),
                _ => (),
            }

            self.advance()?;
        }

        Ok(())
    }

    fn advance(&mut self) -> Result<()> {
        let token = self.scanner.next_token()?;
        let prev = mem::replace(&mut self.current, Some(token));
        self.previous = prev;
        Ok(())
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current().token_type == token_type
    }

    fn match_token(&mut self, token_type: TokenType) -> Result<bool> {
        if !self.check(token_type) {
            Ok(false)
        } else {
            self.advance()?;
            Ok(true)
        }
    }

    fn consume(&mut self, token_type: TokenType, message: String) -> Result<()> {
        let token = self.current();

        if token.token_type == token_type {
            self.advance()
        } else {
            Err(Error::new(message, token.line))
        }
    }

    fn current(&self) -> Token<'src> {
        self.current.expect("Current token cannot be None.")
    }

    fn previous(&self) -> Token<'src> {
        self.previous.expect("Previous token cannot be None.")
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    /// Create an constant that points to an identifier string
    fn identifier_constant(&mut self, name: String) -> Result<u8> {
        let ptr = self.alloc(Object::String(name));
        self.make_constant(ptr)
    }

    fn define_variable(&mut self, global: u8) {
        self.emit_bytes(OpCode::DefineGlobal as u8, global);
    }

    fn make_constant(&mut self, ptr: ObjectPtr) -> Result<u8> {
        let constant = self.chunk.add_constant(ptr);

        if constant > u8::MAX as usize {
            Err(Error::new(
                "Too many constants in one chunk.".to_string(),
                self.previous().line),
            )
        } else {
            Ok(constant as u8)
        }
    }

    fn emit_byte(&mut self, byte: u8) {
        let line = self.previous().line;
        self.current_chunk().write(byte, line);
    }

    fn emit_bytes(&mut self, a: u8, b: u8) {
        self.emit_byte(a);
        self.emit_byte(b);
    }

    fn emit_constant(&mut self, object: Object) -> Result<()> {
        let ptr = self.alloc(object);
        let constant = self.make_constant(ptr)?;
        self.emit_bytes(OpCode::Constant as u8, constant);
        Ok(())
    }

    fn alloc(&mut self, object: Object) -> ObjectPtr {
        let ptr = ObjectPtr::alloc(object);
        self.objects.push(ptr);
        ptr
    }
}
