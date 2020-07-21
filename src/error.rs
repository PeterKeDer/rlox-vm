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
