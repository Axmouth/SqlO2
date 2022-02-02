#[cfg(test)]
mod parser_tests {
    use sqlo2::{
        ast::Ast,
        backend::EvalResult,
        backend_memory::MemoryBackend,
        parser::{Parser, ParsingError},
        sql_types::SqlValue,
        test_impls::{IntoVecContainer, VecContainer},
    };
    use test_macros::test_case;
    use test_util::{run_test, TestSubjectExt};

    fn parser_test_fn(sql: &str, parser: &mut Parser) -> Result<Ast, ParsingError> {
        parser.parse(sql)
    }

    #[test_case("../tests/unit/parser1")]
    fn parser(test: &str) {
        run_test(test, parser_test_fn, Parser::init());
    }

    fn memory_backend_test_fn(
        sql: &str,
        backend: &mut MemoryBackend,
    ) -> Result<VecContainer<EvalResult<SqlValue>>, String> {
        let mut result = backend.eval_query(sql);
        result
            .as_mut()
            .map(|r| r.iter_mut().for_each(|r| r.zero_time()))
            .ok();
        result.map(|r| r.into_vec_container())
    }

    #[test_case("../tests/acceptance/memory1")]
    fn memory_backend(test: &str) {
        run_test(test, memory_backend_test_fn, MemoryBackend::init());
    }
}
