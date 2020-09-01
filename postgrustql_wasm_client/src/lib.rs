#[macro_use]
extern crate lazy_static;

use backend_memory::{MemoryBackend, MemoryCell};
use postgrustql::backend::{Cell, EvalResult};
pub use postgrustql::backend_memory;
use std::sync::Mutex;
use wasm_bindgen::prelude::*;

use js_sys::Array;
use js_sys::Boolean;
use js_sys::JsString;
use js_sys::Number;
use js_sys::{Map, Object};

lazy_static! {
    static ref BACKEND: Mutex<MemoryBackend> = Mutex::new(MemoryBackend::new());
}

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn eval_raw(cmd: String) -> Object {
    let result = eval_query(cmd);

    query_results_to_js(result)
}

pub fn eval_query(cmd: String) -> Result<Vec<EvalResult<MemoryCell>>, String> {
    println!(" println");
    unsafe {
        log("1");
    }
    let backend = &mut BACKEND.lock().unwrap();
    let result = backend.eval_query(&cmd);
    result
}

pub fn query_results_to_js(results: Result<Vec<EvalResult<MemoryCell>>, String>) -> Object {
    let value_map: Map = Map::new();
    match results {
        Err(err) => {
            value_map.set(&JsString::from("success"), &Boolean::from(false));
            value_map.set(&JsString::from("error"), &JsString::from(err));
        }
        Ok(results) => {
            value_map.set(&JsString::from("success"), &Boolean::from(true));
            let result_array = Array::new();
            for result in results {
                let result_map: Map = Map::new();
                match result {
                    EvalResult::Select {
                        results: query_results,
                        time,
                    } => {
                        let col_list = query_results.columns.clone();
                        let query_columns_array: Array = query_results
                            .columns
                            .into_iter()
                            .map(|s| JsString::from(format!("{}({})", s.name, s.col_type)))
                            .collect();
                        let rows_array = Array::new();
                        for row in query_results.rows {
                            let row_array = Array::new();
                            for (i, col) in row.into_iter().enumerate() {
                                match col_list[i].col_type {
                                    postgrustql::backend::ColumnType::TextType => {
                                        row_array.push(&JsString::from(col.as_text().unwrap()));
                                    }
                                    postgrustql::backend::ColumnType::IntType => {
                                        row_array.push(&Number::from(col.as_int().unwrap()));
                                    }
                                    postgrustql::backend::ColumnType::BoolType => {
                                        row_array.push(&Boolean::from(col.as_bool().unwrap()));
                                    }
                                }
                            }
                            rows_array.push(&row_array);
                        }
                        result_map.set(
                            &JsString::from("time"),
                            &JsString::from(format!("{:.2?}", time)),
                        );
                        result_map.set(&JsString::from("rows"), &rows_array);
                        result_map.set(&JsString::from("columns"), &query_columns_array);
                    }
                    EvalResult::Insert { success, time } => {
                        result_map.set(&JsString::from("success"), &Boolean::from(success));
                        result_map.set(
                            &JsString::from("time"),
                            &JsString::from(format!("{:.2?}", time)),
                        );
                    }
                    EvalResult::CreateTable { success, time } => {
                        result_map.set(&JsString::from("success"), &Boolean::from(success));
                        result_map.set(
                            &JsString::from("time"),
                            &JsString::from(format!("{:.2?}", time)),
                        );
                    }
                }
                result_array.push(&Object::from_entries(&result_map.entries()).unwrap());
            }
            value_map.set(&JsString::from("results"), &result_array);
        }
    }

    Object::from_entries(&value_map.entries()).unwrap()
}
