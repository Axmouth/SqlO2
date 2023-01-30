#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

pub mod ast;
pub mod backend;
pub mod backend_memory;
pub mod lexer;
pub mod parser;
pub mod sql_types;

pub mod test_impls;

#[cfg(test)]
mod tests {}
