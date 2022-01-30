#[macro_use]
extern crate lazy_static;

use yew_router::prelude::*;

mod components;
mod services;

use components::app::App;

#[derive(Routable, PartialEq, Clone, Debug)]
pub enum Routes {
    #[at("/")]
    Home,
    #[not_found]
    #[at("/404")]
    NotFound,
}

fn main() {
    wasm_logger::init(wasm_logger::Config::default());

    yew::start_app::<App>();
}
