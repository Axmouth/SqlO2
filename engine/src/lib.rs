#[cfg(not(target_env = "msvc"))]
#[cfg(feature = "jemallocator")]
extern crate jemallocator;

#[cfg(not(target_env = "msvc"))]
#[cfg(feature = "jemallocator")]
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[cfg(feature = "tcmalloc")]
extern crate tcmalloc;

#[cfg(feature = "tcmalloc")]
use tcmalloc::TCMalloc;

#[cfg(feature = "tcmalloc")]
#[global_allocator]
static GLOBAL: TCMalloc = TCMalloc;

pub mod ast;
pub mod backend;
pub mod backend_memory;
pub mod lexer;
pub mod parser;
pub mod sql_types;

#[macro_use]
extern crate lazy_static;
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
