use yew::prelude::*;
use yew_router::prelude::*;

use super::repl::Repl;

#[function_component(App)]
pub fn app() -> Html {
    html! {
        <BrowserRouter>
            <Repl />
        </BrowserRouter>
    }
}
