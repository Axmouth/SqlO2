use std::sync::Mutex;

use sqlo2::{backend::EvalResult, backend_memory::MemoryBackend, sql_types::SqlValue};

lazy_static! {
    static ref BACKEND: Mutex<MemoryBackend> = Mutex::new(MemoryBackend::new());
}

struct SqlO2Service;

impl SqlO2Service {
    pub fn new() -> Self {
        SqlO2Service {}
    }

    pub fn execute(&self, sql: &str) -> Result<Vec<EvalResult<SqlValue>>, String> {
        let mut backend = BACKEND.lock().unwrap();
        let result = backend.eval_query(sql);
        match result {
            Ok(result) => Ok(result),
            Err(err) => Err(format!("{}", err)),
        }
    }
}
