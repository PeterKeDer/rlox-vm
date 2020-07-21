use std::collections::HashMap;
use lazy_static::lazy_static;

use crate::scanner::TokenType;

// Generate constants for keywords
macro_rules! keywords {
    ( $( $value:expr => $name:ident | $upper_name:ident, )* ) => {
        $(
            pub static $upper_name: &'static str = $value;
        )*

        lazy_static! {
            /// Map keyword `String`s to their corresponding `TokenType`
            pub static ref KEYWORD_TOKENS: HashMap<String, TokenType> = {
                let mut m = HashMap::new();
                $(
                    m.insert($value.to_string(), TokenType::$name);
                )*
                m
            };
        }
    }
}

keywords! {
    "and"       => And      | AND,
    "class"     => Class    | CLASS,
    "else"      => Else     | ELSE,
    "false"     => False    | FALSE,
    "fun"       => Fun      | FUN,
    "for"       => For      | FOR,
    "if"        => If       | IF,
    "nil"       => Nil      | NIL,
    "or"        => Or       | OR,
    "print"     => Print    | PRINT,
    "return"    => Return   | RETURN,
    "super"     => Super    | SUPER,
    "this"      => This     | THIS,
    "true"      => True     | TRUE,
    "var"       => Var      | VAR,
    "while"     => While    | WHILE,
}
