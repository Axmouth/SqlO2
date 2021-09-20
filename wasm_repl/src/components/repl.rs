use yew::prelude::*;

#[derive(Debug, Clone)]
pub(crate) enum Msg {
    AddOne,
}

#[derive(Debug, Clone, Default)]
struct ExecutedQuery {}

#[derive(Debug, Clone)]
pub(crate) struct Repl {
    // `ComponentLink` is like a reference to a component.
    // It can be used to send messages to the component
    link: ComponentLink<Self>,
    value: i64,
    exec_history: Vec<ExecutedQuery>,
    query_string: String,
    can_type: bool,
    history_position: u32,
    current: String,
}

impl Component for Repl {
    type Message = Msg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        Self {
            link,
            value: 0,
            exec_history: Vec::new(),
            query_string: "".to_string(),
            can_type: false,
            history_position: 0,
            current: "".to_string(),
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::AddOne => {
                self.value += 1;
                // the value has changed so we need to
                // re-render for it to appear on the page
                true
            }
        }
    }

    fn change(&mut self, _props: Self::Properties) -> ShouldRender {
        // Should only return "true" if new properties are different to
        // previously received properties.
        // This component has no properties so we will always return "false".
        false
    }

    fn view(&self) -> Html {
        html! {
            <div>
                <button onclick=self.link.callback(|_| Msg::AddOne)>{ "+1" }</button>
                <p>{ self.value }</p>
            </div>
        }
    }
}
