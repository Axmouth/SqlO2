pub mod ast;
pub mod backend;
pub mod backend_memory;
pub mod lexer;
pub mod parser;
pub mod repl;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
