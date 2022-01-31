#[cfg(test)]
mod parser_tests {
    use sqlo2::{
        ast::Ast,
        backend::EvalResult,
        backend_memory::MemoryBackend,
        parser::{Parser, ParsingError},
        sql_types::SqlValue,
    };
    use test_macros::test_case;
    use test_util::{run_test, TestSubjectExt};

    #[test_case("../tests/unit/parser1")]
    fn parser(test: &str) {
        fn test_fn(sql: &str, parser: &mut Parser) -> Result<Ast, ParsingError> {
            parser.parse(sql)
        }
        run_test(test, test_fn, Parser::init());
    }

    #[test_case("../tests/acceptance/memory1")]
    fn memory_backend(test: &str) {
        fn test_fn(
            sql: &str,
            backend: &mut MemoryBackend,
        ) -> Result<Vec<EvalResult<SqlValue>>, String> {
            let mut result = backend.eval_query(sql);
            result
                .as_mut()
                .map(|r| r.iter_mut().for_each(|r| r.zero_time()))
                .ok();
            result
        }
        run_test(test, test_fn, MemoryBackend::init());
    }
}
