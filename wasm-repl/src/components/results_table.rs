use sqlo2::{backend::QueryResults, sql_types::SqlValue};
use yew::prelude::*;

pub enum Msg {}

#[derive(Clone, PartialEq, Properties)]
pub struct ResultsTableProps {
    pub result: QueryResults<SqlValue>,
}

pub struct ResultsTable {
    // `ComponentLink` is like a reference to a component.
    // It can be used to send messages to the component
    props: ResultsTableProps,
}

impl Component for ResultsTable {
    type Message = Msg;
    type Properties = ResultsTableProps;

    fn create(props: Self::Properties, _: ComponentLink<Self>) -> Self {
        Self {
            props: ResultsTableProps {
                result: props.result,
            },
        }
    }

    fn update(&mut self, _: Self::Message) -> ShouldRender {
        false
    }

    fn change(&mut self, props: Self::Properties) -> ShouldRender {
        props != self.props
    }

    fn view(&self) -> Html {
        html! {
            <>
            <table>
                <tr>
                    { for self.props.result.columns.iter().map(|column| {
                        html! {
                            <th class="table-header dashed-border">{ format!("{}({})", column.name, column.col_type) }</th>
                        }
                    })
                    }
                </tr>

                { for self.props.result.rows.iter().map(|row| {
                    html! {
                        <tr>
                        { for row.iter().map(|field| {
                            html! {
                                <td class="dashed-border">{ field }</td>
                                }
                            })
                        }
                        </tr>
                    }
                    })
                }
            </table>
            {
                if self.props.result.rows.len() == 1 {
                    html! {
                        <div class="results">{"("}{ self.props.result.rows.len() }{" result)"}</div>
                    }
                } else {
                    html! {
                        <div class="results">{"("}{ self.props.result.rows.len() }{" results)"}</div>
                    }
                }

            }
            </>
        }
    }
}
