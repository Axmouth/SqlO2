#[macro_use]
extern crate lazy_static;

mod components;
mod services;

fn main() {
    yew::start_app::<components::repl::Repl>();
}
