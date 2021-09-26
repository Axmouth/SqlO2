#[macro_use]
extern crate lazy_static;

mod components;
mod services;

fn main() {
    wasm_logger::init(wasm_logger::Config::default());

    yew::start_app::<components::repl::Repl>();
}
