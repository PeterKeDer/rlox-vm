use std::{fmt, error};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Error {
    pub message: String,
    pub line: usize,
}

impl Error {
    pub fn new(message: String, line: usize) -> Error {
        Error {
            message,
            line,
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error [{}]: {}", self.line, self.message)
    }
}

impl error::Error for Error {}
