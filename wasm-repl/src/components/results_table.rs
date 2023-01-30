use sqlo2::{backend::QueryResults, sql_types::SqlValue};
use yew::prelude::*;

#[derive(Clone, PartialEq, Properties)]
pub struct ResultsTableProps {
    pub result: QueryResults<SqlValue>,
}

#[function_component(ResultsTable)]
pub fn results_table(props: &ResultsTableProps) -> Html {
    html! {
        <>
        <table>
            <tr>
                { for props.result.columns.iter().map(|column| {
                    html! {
                        <th class="table-header dashed-border">{ format!("{}({})", column.name, column.col_type) }</th>
                    }
                })
                }
            </tr>

            { for props.result.rows.iter().map(|row| {
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
            if props.result.rows.len() == 1 {
                html! {
                    <div class="results">{"("}{ props.result.rows.len() }{" result)"}</div>
                }
            } else {
                html! {
                    <div class="results">{"("}{ props.result.rows.len() }{" results)"}</div>
                }
            }

        }
        </>
    }
}
