use crate::components::results_table::ResultsTable;
use crate::services::sqlo2_service::SqlO2Service;
use sqlo2::{backend::EvalResult, sql_types::SqlValue};

use serde::{Deserialize, Serialize};
use yew::{
    html, web_sys::HtmlTextAreaElement, Component, ComponentLink, Html, InputData, KeyboardEvent,
    NodeRef, ShouldRender,
};
use yew_router::service::RouteService;

#[derive(Debug, Clone)]
pub enum Msg {
    FocusInput,
    QuerySubmit,
    HistoryUp,
    HistoryDown,
    QueryInputValue(String),
}

#[derive(Debug, Clone)]
struct ExecutedQuery {
    query_string: String,
    query_results: Result<Vec<EvalResult<SqlValue>>, String>,
}

#[derive(Debug, Deserialize, Serialize)]
struct Query {
    default: Option<Option<bool>>,
    q: Option<Option<String>>,
}

#[derive(Debug)]
pub struct Repl {
    // `ComponentLink` is like a reference to a component.
    // It can be used to send messages to the component
    link: ComponentLink<Self>,
    exec_history: Vec<ExecutedQuery>,
    query_string: String,
    can_type: bool,
    history_position: usize,
    current: String,
    focus_ref: NodeRef,
    route_service: RouteService,
}

impl Component for Repl {
    type Message = Msg;
    type Properties = ();

    fn create(_props: Self::Properties, link: ComponentLink<Self>) -> Self {
        let route_service = RouteService::new();

        let q_str = route_service.get_query();
        let q = serde_qs::from_str::<Query>(&q_str);

        let mut exec_history = vec![];
        let mut history_position = 0;
        if let Ok(q) = q {
            if let Some(Some(true)) = q.default {
                for sql in DEFAULT_QUERIES {
                    exec_history.push(ExecutedQuery {
                        query_results: SqlO2Service::execute(*sql),
                        query_string: sql.to_string(),
                    });
                }
                history_position = DEFAULT_QUERIES.len();
            }
            if let Some(Some(sql)) = q.q {
                exec_history.push(ExecutedQuery {
                    query_results: SqlO2Service::execute(&sql),
                    query_string: sql,
                });
                history_position = DEFAULT_QUERIES.len();
            }
        }

        Self {
            link,
            exec_history,
            query_string: "".to_string(),
            can_type: false,
            history_position,
            current: "".to_string(),
            focus_ref: NodeRef::default(),
            route_service,
        }
    }

    fn update(&mut self, msg: Self::Message) -> ShouldRender {
        match msg {
            Msg::FocusInput => {
                if let Some(input) = self.focus_ref.cast::<HtmlTextAreaElement>() {
                    let _ = input.focus().is_ok();
                }
                true
            }
            Msg::QuerySubmit => {
                self.can_type = false;
                if let Some(input) = self.focus_ref.cast::<HtmlTextAreaElement>() {
                    self.exec_history.push(ExecutedQuery {
                        query_string: input.value(),
                        query_results: SqlO2Service::execute(self.query_string.as_str()),
                    });
                    self.current = "".to_string();
                    self.query_string = "".to_string();
                    self.history_position = self.exec_history.len();
                    let _ = input.focus().is_ok();
                    input.scroll_into_view();
                }
                true
            }
            Msg::HistoryUp => {
                if self.history_position > 0 {
                    self.history_position -= 1;
                }
                if self.history_position >= self.exec_history.len() {
                    self.query_string = self.current.clone();
                } else {
                    self.query_string = self.exec_history[self.history_position]
                        .query_string
                        .clone();
                }
                true
            }
            Msg::HistoryDown => {
                if self.history_position < self.exec_history.len() {
                    self.history_position += 1;
                }
                if self.history_position >= self.exec_history.len() {
                    self.query_string = self.current.clone();
                } else {
                    self.query_string = self.exec_history[self.history_position]
                        .query_string
                        .clone();
                }
                true
            }
            Msg::QueryInputValue(e) => {
                if !self.can_type {
                    self.can_type = true;
                } else {
                    self.query_string = e;
                    if self.history_position == self.exec_history.len() {
                        self.current = self.query_string.clone();
                    }
                }
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
        let focus_input = self.link.callback(|_| Msg::FocusInput);

        let keyup = self.link.batch_callback(|e: KeyboardEvent| {
            if e.key() == "Enter" && !e.shift_key() {
                Some(Msg::QuerySubmit)
            } else if e.key() == "ArrowUp" {
                Some(Msg::HistoryUp)
            } else if e.key() == "ArrowDown" {
                Some(Msg::HistoryDown)
            } else {
                None
            }
        });
        let keydown = self.link.batch_callback(|e: KeyboardEvent| {
            if e.key() == "Enter" && !e.shift_key() {
                e.prevent_default();
                None
            } else {
                None
            }
        });

        html! {
            <>
                <div class="repl-main" onclick=focus_input>
                    { for self.exec_history.iter().map(|item| {
                        html! {
                            <div>
                                <div class="prompt-wrapper">
                                    <div class="prompt">{"SqlO2#:\u{00a0}"}</div>
                                    <pre class="executed-query">{{ &item.query_string }}</pre>
                                </div>
                                <br />
                                {
                                    if let Ok(query_results) = &item.query_results {
                                        html!{
                                            <>
                                                <div class="results-section">
                                                    {
                                                                for query_results.iter().map(|query_result|
                                                                html!{
                                                                    {
                                                                        if let EvalResult::Select { results, time: _ } = &query_result {
                                                                            html!{
                                                                                <div class="results-table">
                                                                                    <ResultsTable result={results.clone()} />
                                                                                </div>
                                                                            }
                                                                        } else {
                                                                            html!{<></>
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
                                        html!{
                                            <div class="failed">
                                                <div>{"fail..\u{00a0}"}</div>
                                                <div>{"Error:\u{00a0}"} { err }</div>
                                            </div>
                                        }

                                    } else {
                                        html!{
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
                        oninput=self.link.callback(|e: InputData| Msg::QueryInputValue(e.value))
                        onkeyup=keyup
                        onkeydown=keydown
                        rows=1
                        autofocus=true
                        ref={self.focus_ref.clone()}
                        value={self.query_string.clone()}
                    ></textarea>
                </div>
            </div>
        </>
        }
    }

    fn rendered(&mut self, _first_render: bool) {
        if let Some(input) = self.focus_ref.cast::<HtmlTextAreaElement>() {
            input.set_rows((input.scroll_height() / 18) as u32);
            let _ = input.focus().is_ok();
            input.scroll_into_view();
        }
    }

    fn destroy(&mut self) {}
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
