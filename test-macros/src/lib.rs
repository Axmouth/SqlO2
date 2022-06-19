extern crate proc_macro;
use std::{
    collections::HashMap,
    sync::{Arc, RwLock},
};

use proc_macro::TokenStream;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref COUNTS: Arc<RwLock<HashMap<String, u32>>> = Arc::new(RwLock::new(HashMap::new()));
}

#[proc_macro_attribute]
pub fn test_case(attr: TokenStream, item: TokenStream) -> TokenStream {
    let item_str = item.to_string();
    let mut item_iter = item.into_iter();
    let mut final_code = "".to_string();
    final_code.push_str(item_str.as_str());
    final_code.push_str("\n\n#[test]\n");
    let mut fn_name_opt = None;
    while let Some(item) = item_iter.next() {
        if item.to_string() == "fn" {
            fn_name_opt = Some(
                item_iter
                    .next()
                    .expect("Didn't find function name after fn")
                    .to_string(),
            );
            break;
        }
    }
    let fn_name = fn_name_opt.expect("No function name found in test case");
    let id: u32 = {
        let mut counts_guard = COUNTS.write().expect("Couldn't get write lock on counts");
        let count_opt = counts_guard.get_mut(&fn_name);
        if let Some(count) = count_opt {
            *count += 1;
            *count
        } else {
            counts_guard.insert(fn_name.clone(), 1);
            1
        }
    };
    final_code.push_str(format!("fn {}_{}() {{", fn_name, id).as_str());
    final_code.push_str(fn_name.as_str());
    final_code.push('(');
    final_code.push_str(&attr.to_string());
    final_code.push_str(");}");

    final_code.parse().expect("Couldn't parse test case")
}
