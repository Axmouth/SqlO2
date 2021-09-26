use sqlo2::{backend::EvalResult, sql_types::SqlValue};

use yew::web_sys::HtmlInputElement as InputElement;
use yew::{
    classes, html, Component, ComponentLink, Context, FocusEvent, Html, NodeRef, TargetCast,
};
use yew::{events::KeyboardEvent, Classes};

#[derive(Debug, Clone)]
pub(crate) enum Msg {
    AddOne,
}

#[derive(Debug, Clone)]
struct ExecutedQuery {
    query_string: String,
    query_results: Result<Vec<EvalResult<SqlValue>>, String>,
}

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

    fn view(&self, ctx: &Context<Self>) -> Html {
        html! {
        { for self.exec_history.iter().map(|item| {
            html! {
                <>
                <div onclick="focusInput()">
                    <div class="prompt-wrapper">
                        <div class="prompt">{ "SqlO2#:&nbsp;" }</div>
                        <pre class="executed-query">{{ item.query_string }}</pre>
                    </div>
                    <br />
                    {
                        if let Ok(results) = item.query_results {
                            html!{
                                <>
                                    <div class="results">
                                        { for results.iter().map(|r|
                                            html!{
                                                <div>
                                                    <app-results-table /*[results]="results" *ngIf="results?.rows"*/ class="results-table"></app-results-table>
                                                </div>
                                            })
                                        }
                                    </div>
                                    <div class="ok">
                                        <div>{"ok!"}</div>
                                    </div>
                                </>
                            }
                        } else if let Err(err) = item.query_results {
                            html!{
                                <div class="failed">
                                    <div>{"fail.."}</div>
                                    <div>{"Error:"} { err }</div>
                                </div>
                            }

                        } else {
                            html!{
                                <div class="failed"/>
                            }
                        }
                    }
                    </div>

                    <div class="prompt-wrapper" onclick="focusInput()">
                    <div class="prompt">{"SqlO2#:&nbsp;"}</div>
                    <textarea
                    /*
                        [(ngModel)]="queryString"
                        (keyup.enter)="onQuerySubmit($event)"
                        (keyup.arrowup)="onHistoryUp($event)"
                        (keyup.arrowdown)="onHistoryDown($event)"
                        (keyup)="autoGrow($event)"
                        class="terminal-input"
                        [cdkTextareaAutosize]="false"
                        #promptInput="cdkTextareaAutosize"
                        #promptInputRef
                        cdkTrapFocus
                        [cdkTrapFocusAutoCapture]="true"
                        */
                    ></textarea>
                    </div>
                    </>

        }})


        }
        }
    }

    fn rendered(&mut self, _first_render: bool) {}

    fn destroy(&mut self) {}
}
