use std::rc::Rc;

use crate::components::results_table::ResultsTable;
use crate::services::sqlo2_service::SqlO2Service;
use sqlo2::{backend::EvalResult, sql_types::SqlValue};

use serde::{Deserialize, Serialize};
use wasm_bindgen::JsCast;
use web_sys::{EventTarget, HtmlTextAreaElement};
use yew::prelude::*;
use yew_router::prelude::*;

#[derive(Debug, Clone)]
pub enum ReplAction {
    FocusInput,
    HistoryDown,
    HistoryUp,
    QueryInputValue(String),
    QuerySubmit(String),
}

#[derive(Debug, Clone, PartialEq)]
struct ExecutedQuery {
    query_string: Box<str>,
    query_results: Result<Vec<EvalResult<SqlValue>>, String>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq)]
struct Query {
    default: Option<Option<bool>>,
    q: Option<Option<String>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ReplState {
    exec_history: Vec<ExecutedQuery>,
    query_string: Box<str>,
    can_type: bool,
    history_position: usize,
    current: Box<str>,
    focus_ref: NodeRef,
}

impl Reducible for ReplState {
    type Action = ReplAction;

    fn reduce(self: Rc<Self>, msg: Self::Action) -> Rc<Self> {
        match msg {
            ReplAction::FocusInput => {
                if let Some(input) = self.focus_ref.cast::<HtmlTextAreaElement>() {
                    let _ = input.focus().is_ok();
                    input.scroll_into_view();
                }

                self
            }
            ReplAction::QuerySubmit(sql) => {
                let mut state = (*self).clone();
                state.can_type = false;
                state.exec_history.push(ExecutedQuery {
                    query_string: sql.clone().into(),
                    query_results: SqlO2Service::execute(&sql),
                });
                state.current = "".into();
                state.query_string = "".into();
                state.history_position = state.exec_history.len();

                Rc::new(state)
            }
            ReplAction::HistoryUp => {
                let mut state = (*self).clone();
                if state.history_position > 0 {
                    state.history_position -= 1;
                }

                state.query_string = state
                    .exec_history
                    .get(state.history_position)
                    .map(|e| e.query_string.clone())
                    .unwrap_or_else(|| state.current.clone());

                Rc::new(state)
            }
            ReplAction::HistoryDown => {
                let mut state = (*self).clone();
                if state.history_position < state.exec_history.len() {
                    state.history_position += 1;
                }

                state.query_string = state
                    .exec_history
                    .get(state.history_position)
                    .map(|e| e.query_string.clone())
                    .unwrap_or_else(|| state.current.clone());

                Rc::new(state)
            }
            ReplAction::QueryInputValue(e) => {
                let mut state = (*self).clone();
                if !state.can_type {
                    state.can_type = true;
                } else {
                    state.query_string = e.into();
                    if state.history_position == state.exec_history.len() {
                        state.current = state.query_string.clone();
                    }
                }

                Rc::new(state)
            }
        }
    }
}

#[function_component(Repl)]
pub fn repl() -> Html {
    let focus_ref = use_node_ref();
    let state = use_reducer(|| ReplState {
        focus_ref: focus_ref.clone(),
        ..Default::default()
    });

    let location = use_location();
    let query: Option<Query> = location.and_then(|loc| loc.query().ok());

    let state_cb = state.clone();
    use_effect_with_deps(
        move |_| {
            if let Some(q) = query {
                if let Some(Some(true)) = q.default {
                    for sql in DEFAULT_QUERIES.iter() {
                        state_cb.dispatch(ReplAction::QuerySubmit(sql.to_string()));
                    }
                }
                if let Some(Some(sql)) = q.q {
                    state_cb.dispatch(ReplAction::QuerySubmit(sql));
                }
                state_cb.dispatch(ReplAction::FocusInput);
            }

            || {}
        },
        (),
    );

    let state_cb = state.clone();
    use_effect(move || {
        move || {
            if let Some(input) = state_cb.focus_ref.cast::<HtmlTextAreaElement>() {
                input.set_rows((input.scroll_height() / 18) as u32);
                let _ = input.focus().is_ok();
                input.scroll_into_view();
            }
        }
    });

    let state_cb = state.clone();
    let focus_input = Callback::from(move |_| {
        state_cb.dispatch(ReplAction::FocusInput);
    });

    let state_cb = state.clone();
    let keyup = Callback::from(move |e: KeyboardEvent| {
        if e.key() == "Enter" && !e.shift_key() {
            state_cb.dispatch(ReplAction::QuerySubmit(state_cb.query_string.to_string()));
            state_cb.dispatch(ReplAction::FocusInput);
        } else if e.key() == "ArrowUp" {
            state_cb.dispatch(ReplAction::HistoryUp);
        } else if e.key() == "ArrowDown" {
            state_cb.dispatch(ReplAction::HistoryDown);
        }
    });

    let state_cb = state.clone();
    let keydown = Callback::from(move |e: KeyboardEvent| {
        if e.key() == "Enter" && !e.shift_key() {
            e.prevent_default();
        }
        let target: Option<EventTarget> = e.target();
        let input = target.and_then(|t| t.dyn_into::<HtmlTextAreaElement>().ok());
        if let Some(value) = input.map(|input| input.value()) {
            state_cb.dispatch(ReplAction::QueryInputValue(value));
        }
    });

    html! {
        <>
            <div class="repl-main" onclick={focus_input}>
                { for state.exec_history.iter().map(|item| {
                    html! {
                        <div>
                            <div class="prompt-wrapper">
                                <div class="prompt">{"SqlO2#:\u{00a0}"}</div>
                                <pre class="executed-query">{{ &item.query_string }}</pre>
                            </div>
                            <br />
                            {
                                if let Ok(query_results) = &item.query_results {
                                    html! {
                                        <>
                                            <div class="results-section">
                                                {
                                                    for query_results.iter().map(|query_result|
                                                        html! {
                                                            {
                                                                if let EvalResult::Select { results, time: _ } = &query_result {
                                                                    html! {
                                                                        <div class="results-table">
                                                                            <ResultsTable result={results.clone()} />
                                                                        </div>
                                                                    }
                                                                } else {
                                                                    html! {
                                                                        <></>
                                                                    }
                                                                }
                                                            }
                                                        })
                                                }
                                            </div>
                                            <div class="ok">
                                                <div>{"ok!"}</div>
                                            </div>
                                        </>
                                    }
                                } else if let Err(err) = &item.query_results {
                                    html! {
                                        <div class="failed">
                                            <div>{"fail..\u{00a0}"}</div>
                                            <div>{"Error:\u{00a0}"} { err }</div>
                                        </div>
                                    }
                                } else {
                                    html! {
                                        <div class="failed"/>
                                    }
                                }
                            }
                            </div>
                }})

                }

            <div class="prompt-wrapper">
            <div class="prompt">{"SqlO2#:\u{00a0}"}</div>
                <textarea
                    class="terminal-input"
                    onkeyup={keyup}
                    onkeydown={keydown}
                    rows=1
                    autofocus=true
                    ref={focus_ref.clone()}
                    value={state.query_string.to_string()}
                >
                </textarea>
            </div>
        </div>
    </>
    }
}

static DEFAULT_QUERIES: &[&str] = &[
    "CREATE TABLE characters (id INT PRIMARY KEY, name TEXT);",
    "INSERT INTO characters VALUES (0, 'Phantaminum');",
    "INSERT INTO characters VALUES (1, 'The 25th Bam');",
    "INSERT INTO characters VALUES (2, 'Rachel');",
    "INSERT INTO characters VALUES (3, 'Rak WraithKaiser');",
    "INSERT INTO characters VALUES (4, 'Khun Aguero Agnes');",
    "INSERT INTO characters VALUES (5, 'King Zahard');",
    "INSERT INTO characters VALUES (6, 'Ha Yuri Zahard');",
    "INSERT INTO characters VALUES (7, 'Androssi Zahard');",
    "INSERT INTO characters VALUES (7, 'Evankhell');",
    "INSERT INTO characters VALUES (8, 'Evankhell');",
    "INSERT INTO characters VALUES (9, 'Anak Zahard');",
    "INSERT INTO characters VALUES (10, 'Yeon Yihwa');",
    "INSERT INTO characters VALUES (11, 'Yeo Miseng');",
    "INSERT INTO characters VALUES (12, 'Yeo Goseng');",
    "INSERT INTO characters VALUES (13, 'Xia Xia');",
    "INSERT INTO characters VALUES (14, 'Sachi Faker');",
    "INSERT INTO characters VALUES (15, 'Hwa Ryun');",
    "INSERT INTO characters VALUES (16, 'Khun Ran');",
    "INSERT INTO characters VALUES (17, 'Ha Yura');",
    "SELECT id, name FROM characters;",
    "SELECT id, name FROM characters WHERE id != 2;",
    "SELECT id, name FROM characters WHERE name = 'Rachel'",
    "SELECT id, name as charName FROM characters WHERE name != 'Rachel' AND id < 5;",
    "SELECT name FROM characters ORDER BY name ASC",
    "SELECT DISTINCT (id / 2)::int FROM characters",
    "SELECT id::text || ' ' || name AS name_with_id FROM characters WHERE id > 1 ORDER BY id DESC LIMIT 4 OFFSET 5;",
    "CREATE TABLE character_roles (character_id INT, role_name TEXT); INSERT INTO character_roles VALUES (1, 'Wave Controller'); INSERT INTO character_roles VALUES (2, 'Light Bearer'); INSERT INTO character_roles VALUES (3, 'Spear Bearer'); INSERT INTO character_roles VALUES (4, 'Light Bearer'); INSERT INTO character_roles VALUES (1, 'Fisherman'); INSERT INTO character_roles VALUES (4, 'Spear Bearer');",
    "SELECT * FROM characters INNER JOIN character_roles ON characters.id=character_roles.character_id WHERE id != 2 ORDER BY id;",
];
