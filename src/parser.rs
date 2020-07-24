use std::mem;
use std::convert::TryFrom;

use num_enum::TryFromPrimitive;

use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, OpCode};
use crate::object::{ObjectPtr, Object, Function, FunctionType};
use crate::error::{Error, Result};
use crate::allocator::ObjectAllocator;
use crate::compiler::{Compiler, Local};

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
        impl Parser<'_, '_> {
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

// Implement function `get_precedence`, `parse_prefix`, and `parse_infix` for `Parser`.
// Format: `token_type` => (`prefix`, `infix`, `precedence`).
// `prefix` and `infix` correspond to member functions belonging to `Parser`.
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
    And             => (none,       and,        And         ),
    Class           => (none,       none,       None        ),
    Else            => (none,       none,       None        ),
    False           => (literal,    none,       None        ),
    For             => (none,       none,       None        ),
    Fun             => (none,       none,       None        ),
    If              => (none,       none,       None        ),
    Nil             => (literal,    none,       None        ),
    Or              => (none,       or,         Or          ),
    Print           => (none,       none,       None        ),
    Return          => (none,       none,       None        ),
    Super           => (none,       none,       None        ),
    This            => (none,       none,       None        ),
    True            => (literal,    none,       None        ),
    Var             => (none,       none,       None        ),
    While           => (none,       none,       None        ),
    EOF             => (none,       none,       None        ),
}

/// Match a `TokenType`. If any branch matched, `Parser.advance` is called first.
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

pub struct Parser<'src, 'alloc> {
    allocator: &'alloc mut dyn ObjectAllocator,
    scanner: Scanner<'src>,
    compiler: Compiler<'src>,
    current: Option<Token<'src>>,
    previous: Option<Token<'src>>,
    precedence: Precedence,
    has_error: bool,
}

impl Parser<'_, '_> {
    /// Compile `Token`s from `scanner` and return the `Chunk` produced and the `ObjectPtr`s on the heap.
    pub fn compile(scanner: Scanner<'_>, allocator: &'_ mut dyn ObjectAllocator) -> Result<Function> {
        let mut parser = Parser {
            allocator,
            scanner,
            current: None,
            compiler: Compiler::new(FunctionType::Script),
            previous: None,
            precedence: Precedence::None,
            has_error: false,
        };

        // This initializes `current` field of `parser` by calling `next_token` on `scanner`
        parser.advance()?;

        // Parses and compiles a program
        parser.program()?;

        if parser.has_error {
            return Err(Error::new("At least one error has occured.".to_string(), 1));
        }

        Ok(parser.compiler.function)
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
            If => self.if_statement(),
            While => self.while_statement(),
            For => self.for_statement(),
            LeftBrace => self.block(),
            _ => self.expression_statement(),
        })
    }

    fn block(&mut self) -> Result<()> {
        self.begin_scope();

        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.declaration()?;
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.".to_string())?;
        self.end_scope();

        Ok(())
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

    fn if_statement(&mut self) -> Result<()> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.".to_string())?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.".to_string())?;

        // Jump to else branch
        let then_jump = self.emit_jump(OpCode::JumpIfFalse as u8);

        // Pop value remaining from jump, if condition is true
        self.emit_byte(OpCode::Pop as u8);

        // Then branch
        self.statement()?;

        // Jump before else branch to the end
        let else_jump = self.emit_jump(OpCode::Jump as u8);
        self.patch_jump(then_jump)?;

        // Pop value remaining from jump, if condition is false
        self.emit_byte(OpCode::Pop as u8);

        if self.match_token(TokenType::Else)? {
            // Else branch
            self.statement()?;
        }

        self.patch_jump(else_jump)?;

        Ok(())
    }

    fn while_statement(&mut self) -> Result<()> {
        let loop_start = self.current_chunk().code.len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.".to_string())?;
        self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.".to_string())?;

        // Jump to exit the loop if condition is false
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse as u8);

        self.emit_byte(OpCode::Pop as u8);
        self.statement()?;

        self.emit_loop(loop_start)?;

        self.patch_jump(exit_jump)?;
        self.emit_byte(OpCode::Pop as u8);

        Ok(())
    }

    fn for_statement(&mut self) -> Result<()> {
        // Note that for loop will create an addition scope (for the potential new variable)
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.".to_string())?;

        // Match nothing, variable declaration, or an expression
        match_advance!(self, {
            Semicolon => Ok(()),
            Var => self.variable_declaration(),
            _ => self.expression_statement(),
        })?;

        // The place to jump back for each loop iteration
        let mut loop_start = self.current_chunk().code.len();

        // Match optional loop condition
        let mut exit_jump = None;
        if !self.match_token(TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.".to_string())?;

            // Exit when condition is false
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse as u8));

            // Pop condition value
            self.emit_byte(OpCode::Pop as u8);
        }

        // Match optional increment clause
        if !self.match_token(TokenType::RightParen)? {
            // Increment clause is executed after the loop, so jump to execute body first
            let body_jump = self.emit_jump(OpCode::Jump as u8);

            // Record location to jump to increment clause
            let loop_increment = self.current_chunk().code.len();

            self.expression()?;
            self.emit_byte(OpCode::Pop as u8);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.".to_string())?;

            // Jump back to condition after executing clause
            self.emit_loop(loop_start)?;

            // Change loop start to jump to increment clause
            loop_start = loop_increment;

            self.patch_jump(body_jump)?;
        }

        self.statement()?;

        // Jump to loop start, whether it be increment clause or condition
        self.emit_loop(loop_start)?;

        if let Some(jump) = exit_jump {
            self.patch_jump(jump)?;
            self.emit_byte(OpCode::Pop as u8);
        }

        self.end_scope();

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

    fn and(&mut self) -> Result<()> {
        // The `and` operator evaluates to the left side if it is falsey, otherwise the right side
        // Jump to end if the value is falsey
        let end_jump = self.emit_jump(OpCode::JumpIfFalse as u8);

        // If did not jump to end, the bool value is no longer needed
        self.emit_byte(OpCode::Pop as u8);

        self.parse_precedence(Precedence::And)?;
        self.patch_jump(end_jump)?;

        Ok(())
    }

    fn or(&mut self) -> Result<()> {
        // The `or` operator evaluates to the left side if it is truthy, otherwise the right side
        // If value is falsey, expression
        let else_jump = self.emit_jump(OpCode::JumpIfFalse as u8);

        // Jump to end since value is truthy
        let end_jump = self.emit_jump(OpCode::Jump as u8);

        self.patch_jump(else_jump)?;
        self.emit_byte(OpCode::Pop as u8);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(end_jump)?;

        Ok(())
    }

    fn variable(&mut self) -> Result<()> {
        self.named_variable(self.previous())
    }

    fn string(&mut self) -> Result<()> {
        let token = self.previous();
        let string = token.lexeme[1..token.lexeme.len() - 1].to_string();
        let value = Object::String(string);
        self.emit_constant(value)?;
        Ok(())
    }

    fn number(&mut self) -> Result<()> {
        let token = self.previous();

        let value = token.lexeme
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
    fn named_variable(&mut self, name: Token<'_>) -> Result<()> {
        let (get_op, set_op, arg) = match self.resolve_local(name)? {
            Some(index) => (OpCode::GetLocal, OpCode::SetLocal, index as u8),
            None => (OpCode::GetGlobal, OpCode::SetGlobal, self.identifier_constant(name.lexeme.to_string())?),
        };

        if self.precedence <= Precedence::Assignment && self.match_token(TokenType::Equal)? {
            self.expression()?;
            self.emit_bytes(set_op as u8, arg);
        } else {
            self.emit_bytes(get_op as u8, arg);
        }

        Ok(())
    }

    /// Parse a variable and return the constant address to its name (String).
    fn parse_variable(&mut self, message: String) -> Result<u8> {
        self.consume(TokenType::Identifier, message)?;

        self.declare_variable()?;
        if self.compiler.scope_depth > 0 {
            return Ok(0);
        }

        self.identifier_constant(self.previous().lexeme.to_string())
    }
}

impl<'src> Parser<'src, '_> {
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

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        // Pop all local variables from last scope
        while !self.compiler.locals.is_empty() {
            if let Some(depth) = self.compiler.locals.last().unwrap().depth {
                if depth <= self.compiler.scope_depth {
                    // Stop popping variables from this scope or lower
                    break;
                }
            } else {
                // No depth: uninitialized variable
                break;
            }

            self.compiler.locals.pop();
            self.emit_byte(OpCode::Pop as u8);
        }
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
        self.compiler.chunk()
    }

    /// Create an constant that points to an identifier string
    fn identifier_constant(&mut self, name: String) -> Result<u8> {
        let ptr = self.alloc(Object::String(name));
        self.make_constant(ptr)
    }

    fn declare_variable(&mut self) -> Result<()> {
        if self.compiler.scope_depth == 0 {
            // Global variables are implicitly declared
            return Ok(());
        }

        let name = self.previous();

        for local in self.compiler.locals.iter().rev() {
            if let Some(depth) = local.depth {
                if depth < self.compiler.scope_depth {
                    break;
                }
            }

            if name.lexeme == local.name {
                return Err(Error::new(
                    format!("Variable with name '{}' already declared in the scope.", name.lexeme),
                    name.line,
                ));
            }
        }

        self.add_local(name)?;
        Ok(())
    }

    fn add_local(&mut self, name: Token<'src>) -> Result<()> {
        // There are a max number of local variables (for now) since bytecode are u8
        if self.compiler.locals.len() > u8::MAX as usize {
            return Err(Error::new(
                "Too many local variables.".to_string(),
                name.line,
            ));
        }

        let local = Local::new(name.lexeme, None);
        self.compiler.locals.push(local);
        Ok(())
    }

    fn resolve_local(&mut self, name: Token<'_>) -> Result<Option<usize>> {
        for (index, local) in self.compiler.locals.iter().enumerate().rev() {
            if name.lexeme == local.name {
                if local.depth.is_none() {
                    return Err(Error::new(
                        format!("Cannot use variable '{}' in its own initializer.", name.lexeme),
                        name.line,
                    ));
                } else {
                    return Ok(Some(index));
                }
            }
        }

        Ok(None)
    }

    fn define_variable(&mut self, global: u8) {
        if self.compiler.scope_depth > 0 {
            // Local variable, mark as initialized
            self.compiler.locals.last_mut().expect("Expect declared local variable.").depth = Some(self.compiler.scope_depth);
        } else {
            self.emit_bytes(OpCode::DefineGlobal as u8, global);
        }
    }

    fn make_constant(&mut self, ptr: ObjectPtr) -> Result<u8> {
        let constant = self.current_chunk().add_constant(ptr);

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
        self.compiler.emit_byte(byte, line);
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

    fn emit_jump(&mut self, instruction: u8) -> usize {
        self.emit_byte(instruction);
        self.emit_bytes(0, 0);
        self.current_chunk().code.len() - 2
    }

    fn patch_jump(&mut self, offset: usize) -> Result<()> {
        // -2 to adjust for bytecode for jump offset itself
        let jump = self.current_chunk().code.len() - offset - 2;

        if jump > u16::MAX as usize {
            return Err(Error::new(
                "Too much code to jump over.".to_string(),
                self.current_chunk().get_line(offset),
            ));
        }

        self.current_chunk().code[offset] = (jump >> 8) as u8;
        self.current_chunk().code[offset + 1] = jump as u8;

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> Result<()> {
        self.emit_byte(OpCode::Loop as u8);

        let jump = self.current_chunk().code.len() - loop_start + 2;

        if jump > u16::MAX as usize {
            return Err(Error::new(
                "Loop body too large.".to_string(),
                self.current_chunk().get_line(loop_start),
            ));
        }

        self.emit_bytes((jump >> 8) as u8, jump as u8);

        Ok(())
    }

    fn alloc(&mut self, object: Object) -> ObjectPtr {
        self.allocator.allocate(object)
    }
}
