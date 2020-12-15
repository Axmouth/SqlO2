#[macro_use]
extern crate lazy_static;

use backend_memory::MemoryBackend;
pub use postgrustql::backend_memory;
use postgrustql::{backend::EvalResult, sql_types::SqlValue};
use std::sync::Mutex;
use wasm_bindgen::prelude::*;

use serde::{Deserialize, Serialize};

lazy_static! {
    static ref BACKEND: Mutex<MemoryBackend> = Mutex::new(MemoryBackend::new());
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize, Serialize)]
struct JSResults {
    success: bool,
    results: Option<Vec<JSQueryResults>>,
    error: Option<String>,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Deserialize, Serialize)]
struct JSQueryResults {
    success: bool,
    time: String,
    rows: Option<Vec<Vec<SqlValue>>>,
    columns: Option<Vec<String>>,
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn eval_raw(cmd: String) -> JsValue {
    let result = eval_query(cmd);

    query_results_to_js(result)
}

pub fn eval_query(cmd: String) -> Result<Vec<EvalResult<SqlValue>>, String> {
    let backend = &mut BACKEND.lock().unwrap();
    let result = backend.eval_query(&cmd);
    result
}

pub fn query_results_to_js(results: Result<Vec<EvalResult<SqlValue>>, String>) -> JsValue {
    let result = match results {
        Err(err) => JSResults {
            success: false,
            results: None,
            error: Some(err),
        },
        Ok(results) => JSResults {
            success: true,
            error: None,
            results: Some(
                results
                    .iter()
                    .map(|r| match r {
                        EvalResult::Select { results, time } => JSQueryResults {
                            success: true,
                            rows: Some(results.rows.clone()),
                            time: format!("{:.2?}", time),
                            columns: Some(
                                results
                                    .columns
                                    .iter()
                                    .map(|s| format!("{}({})", s.name, s.col_type))
                                    .collect(),
                            ),
                        },
                        EvalResult::Insert { success, time } => JSQueryResults {
                            success: *success,
                            rows: None,
                            time: format!("{:.2?}", time),
                            columns: None,
                        },
                        EvalResult::CreateTable { success, time } => JSQueryResults {
                            success: *success,
                            rows: None,
                            time: format!("{:.2?}", time),
                            columns: None,
                        },
                        EvalResult::DropTable { success, time } => JSQueryResults {
                            success: *success,
                            rows: None,
                            time: format!("{:.2?}", time),
                            columns: None,
                        },
                    })
                    .collect(),
            ),
        },
    };
    JsValue::from_serde(&result).unwrap()
}
