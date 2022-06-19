use crate::{
    ast::{
        Ast, BinaryExpression, CreateIndexStatement, CreateTableStatement, DropTableStatement,
        Expression, InsertStatement, LiteralExpression, RowDataSource, SelectStatement, Statement,
        TableColumn, UnaryExpression,
    },
    backend::EvalResult,
    parser::ParsingError,
};
use std::fmt::Write;
use test_util::{TestResultExt, TestResultType, TestStringify};
use tree_display::{Context, TreeDisplay};

const INDENT: &str = "   ";
const INDENT_WITH_BRANCH: &str = "|  ";

#[derive(Debug, PartialEq, Eq)]
enum BranchPosition {
    Last,
    First,
    Other,
}

impl BranchPosition {
    pub fn last(&self) -> bool {
        &BranchPosition::Last == self
    }

    pub fn right(&self) -> bool {
        &BranchPosition::First == self
    }
}

pub struct VecContainer<T>(Vec<T>);

pub trait IntoVecContainer<T> {
    fn into_vec_container(self) -> VecContainer<T>;
}

impl<T> IntoVecContainer<T> for Vec<T> {
    fn into_vec_container(self) -> VecContainer<T> {
        VecContainer(self)
    }
}

impl TestStringify for Ast {
    fn stringify(&self) -> String {
        let mut out = "".to_string();
        let ctx = Context {
            indent: "   ",
            ..Context::new()
        };
        for statement in &self.statements {
            match statement {
                Statement::CreateTableStatement(create_table) => {
                    out.push_str("Create Table\n");
                    out.push_str(&create_table.tree_print(ctx, Default::default()));
                    out.push('\n');
                }
                Statement::InsertStatement(insert) => {
                    out.push_str("Insert\n");
                    out.push_str(&insert.tree_print(ctx, Default::default()));
                    out.push('\n');
                }
                Statement::SelectStatement(select) => {
                    out.push_str("Select\n");
                    out.push_str(&select.tree_print(ctx, Default::default()));
                    out.push('\n');
                }
                Statement::DropTableStatement(drop_table) => {
                    out.push_str("Drop Table\n");
                    out.push_str(&drop_table.tree_print(ctx, Default::default()));
                    out.push('\n');
                }
                Statement::CreateIndexStatement(create_index) => {
                    out.push_str("Create Index\n");
                    out.push_str(&create_index.tree_print(ctx, Default::default()));
                    out.push('\n');
                }
            }
        }
        out
    }
}

impl TestStringify for ParsingError {
    fn stringify(&self) -> String {
        format!("{:#?}", self)
    }
}

impl<T> TestStringify for VecContainer<T>
where
    T: std::fmt::Display,
{
    fn stringify(&self) -> String {
        let mut out = String::new();
        for item in self.0.iter() {
            writeln!(out, "{}", item).ok();
        }
        out
    }
}

impl<T> std::fmt::Display for VecContainer<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in self.0.iter() {
            writeln!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl<T> TestResultExt for VecContainer<T>
where
    T: std::fmt::Display,
{
    fn result_type(&self) -> TestResultType {
        TestResultType::Unknown
    }

    fn stringified(&self) -> String {
        self.stringify()
    }
}

impl<C> std::fmt::Display for EvalResult<C>
where
    C: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalResult::CreateTable { success, .. } => {
                if *success {
                    writeln!(f, "CreateTable: success")
                } else {
                    writeln!(f, "CreateTable: failure")
                }
            }
            EvalResult::Select { results, .. } => {
                let mut col_lengths: Vec<usize> = results
                    .columns
                    .iter()
                    .map(|c| c.col_type.to_string().len() + c.name.len() + 2)
                    .collect();
                results.rows.iter().for_each(|r| {
                    r.iter()
                        .zip(col_lengths.iter_mut())
                        .for_each(|(col, col_len)| {
                            let len = col.to_string().len();
                            if len > *col_len {
                                *col_len = len;
                            }
                        });
                });
                let columns = results
                    .columns
                    .iter()
                    .zip(col_lengths.iter())
                    .map(|(c, col_len)| {
                        format!(
                            "{:width$}",
                            format!("{}({})", c.name, c.col_type),
                            width = col_len
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(" | ");

                writeln!(f, "Select:\n| {} |", columns)?;

                let columns_sep = col_lengths
                    .iter()
                    .map(|col_len| "-".repeat(*col_len))
                    .collect::<Vec<_>>()
                    .join("-|-");
                writeln!(f, "|-{}-|", columns_sep)?;
                for result in results.rows.iter() {
                    let column: String = result
                        .iter()
                        .zip(col_lengths.iter())
                        .map(|(col, col_len)| {
                            format!("{:width$}", col.to_string(), width = col_len)
                        })
                        .collect::<Vec<String>>()
                        .join(" | ");
                    writeln!(f, "| {} |", column)?;
                }
                Ok(())
            }
            EvalResult::Insert { success, .. } => {
                if *success {
                    writeln!(f, "Insert: success")
                } else {
                    writeln!(f, "Insert: failure")
                }
            }
            EvalResult::DropTable { success, .. } => {
                if *success {
                    writeln!(f, "DropTable: success")
                } else {
                    writeln!(f, "DropTable: failure")
                }
            }
        }
    }
}

// impl std::fmt::Display for Ast {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         for statement in &self.statements {
//             writeln!(f, "{}", statement)?;
//         }

//         Ok(())
//     }
// }

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_statement(f, self, vec![])
    }
}

fn format_statement(
    f: &mut std::fmt::Formatter<'_>,
    statement: &Statement,
    _indent: Vec<&str>,
) -> std::fmt::Result {
    match statement {
        Statement::CreateTableStatement(CreateTableStatement { name, cols }) => {
            writeln!(f, "Create Table")?;
            writeln!(f, "   ├──Name: {}", name)?;
            writeln!(f, "   └──Columns")?;
            for (i, col) in cols.iter().enumerate() {
                if i < cols.len() - 1 {
                    writeln!(f, "      ├──{}", col.data_type)?;
                } else {
                    writeln!(f, "      └──{}", col.data_type)?;
                }
            }
        }
        Statement::InsertStatement(InsertStatement { table, values }) => {
            writeln!(f, "Insert")?;
            writeln!(f, "   ├──Name: {}", table)?;
            writeln!(f, "   └──Values")?;
            for (i, expr) in values.iter().enumerate() {
                if i + 1 < values.len() {
                    format_expression(f, expr, vec![INDENT, INDENT], BranchPosition::Other)?;
                } else {
                    format_expression(f, expr, vec![INDENT, INDENT], BranchPosition::Last)?;
                }
            }
        }
        Statement::DropTableStatement(DropTableStatement { name }) => {
            writeln!(f, "Drop Table")?;
            writeln!(f, "   └───Name: {}", name)?;
        }
        Statement::SelectStatement(SelectStatement {
            where_clause,
            order_by,
            from,
            is_distinct,
            items,
            limit,
            offset,
        }) => {
            writeln!(f, "Select")?;
            writeln!(f, "   ├──From")?;
            for (i, table) in from.iter().enumerate() {
                if i < from.len() - 1 {
                    write!(f, "  |   ├──")?;
                } else {
                    write!(f, "   |  └──")?;
                }
                match table {
                    RowDataSource::Table {
                        table_name,
                        as_clause: _,
                        joins: _,
                    } => {
                        write!(f, "{}", table_name)?;
                    }
                    RowDataSource::SubSelect {
                        select: _,
                        as_clause: _,
                        joins: _,
                    } => {}
                }
                writeln!(f)?;
            }
            if where_clause != &Expression::Empty {
                writeln!(f, "   ├──Where")?;
                format_expression(
                    f,
                    where_clause,
                    vec![INDENT, INDENT_WITH_BRANCH],
                    BranchPosition::Last,
                )?
            };
            if let Some(order_by) = order_by {
                writeln!(f, "   ├──Order By")?;
                if order_by.asc {
                    writeln!(f, "   |  ├──Asc")?;
                } else {
                    writeln!(f, "   |  ├──Desc")?;
                }
                format_expression(
                    f,
                    &order_by.exp,
                    vec![INDENT, INDENT_WITH_BRANCH],
                    BranchPosition::Last,
                )?;
            }
            if *is_distinct {
                writeln!(f, "   ├──Distinct")?;
            }
            if let Some(limit) = limit {
                writeln!(f, "   ├──Limit")?;
                writeln!(f, "   |  └──{}", limit)?;
            }
            if let Some(offset) = offset {
                writeln!(f, "   ├──Offset")?;
                writeln!(f, "   |  └──{}", offset)?;
            }
            writeln!(f, "   └──Items")?;
            for (i, item) in items.iter().enumerate() {
                if i < items.len() - 1 {
                    writeln!(f, "      ├──Item")?;
                } else {
                    writeln!(f, "      └──Item")?;
                }
                if item.asterisk && item.as_clause.is_some() && item.expression != Expression::Empty
                {
                    writeln!(f, "         ├──Asterisk")?;
                } else if item.asterisk {
                    writeln!(f, "         └──Asterisk")?;
                }
                if let Some(as_clause) = &item.as_clause {
                    if item.expression != Expression::Empty {
                        writeln!(f, "         ├──As")?;
                        writeln!(f, "         |  └──{}", as_clause)?;
                    } else {
                        writeln!(f, "         └──As")?;
                        writeln!(f, "            └──{}", as_clause)?;
                    }
                }
                if item.expression != Expression::Empty {
                    if i < items.len() - 1 {
                        writeln!(f, "      |  └──Expression")?;
                        format_expression(
                            f,
                            &item.expression,
                            vec![INDENT, INDENT, INDENT_WITH_BRANCH, INDENT],
                            BranchPosition::Last,
                        )?;
                    } else {
                        writeln!(f, "         └──Expression")?;
                        format_expression(
                            f,
                            &item.expression,
                            vec![INDENT, INDENT, INDENT, INDENT],
                            BranchPosition::Last,
                        )?;
                    }
                }
            }
        }
        Statement::CreateIndexStatement(CreateIndexStatement {
            table: _,
            name: _,
            is_unique: _,
            is_primary_key: _,
            expression: _,
        }) => {}
    }

    Ok(())
}

fn format_expression(
    f: &mut std::fmt::Formatter<'_>,
    expr: &Expression,
    indent: Vec<&str>,
    pos: BranchPosition,
) -> std::fmt::Result {
    indent.iter().for_each(|indent| {
        let _ = write!(f, "{}", indent);
    });
    if pos.last() {
        write!(f, "└──")?;
    } else if pos.right() {
        write!(f, "|└─")?;
    } else {
        write!(f, "├──")?;
    }
    match expr {
        Expression::TableColumn(TableColumn {
            table_name,
            col_name,
        }) => {
            if let Some(table_name) = table_name {
                write!(f, "{}.", table_name)?
            }
            writeln!(f, "{}", col_name)?;
        }
        Expression::Literal(literal) => match literal {
            LiteralExpression::String(s) => writeln!(f, "\"{}\"", s)?,
            LiteralExpression::Numeric(s) => writeln!(f, "{}", s)?,
            LiteralExpression::Bool(b) => writeln!(f, "{}", b)?,
            LiteralExpression::Identifier(s) => writeln!(f, "{}", s)?,
            LiteralExpression::Null => writeln!(f, "null")?,
        },
        Expression::Binary(BinaryExpression {
            first,
            second,
            operand,
        }) => {
            let mut indent = indent.clone();
            if pos.last() {
                indent.push(INDENT);
            } else {
                indent.push(INDENT_WITH_BRANCH);
            }
            writeln!(f, "{:#?}", operand)?;
            format_expression(f, first, indent.clone(), BranchPosition::First)?;
            format_expression(f, second, indent, BranchPosition::Last)?;
        }
        Expression::Unary(UnaryExpression { first, operand }) => {
            let mut indent = indent.clone();
            if pos.last() {
                indent.push(INDENT);
            } else {
                indent.push(INDENT_WITH_BRANCH);
            }
            indent.iter().for_each(|indent| {
                let _ = write!(f, "{}", indent);
            });
            writeln!(f, "{:#?}", operand)?;
            indent.iter().for_each(|indent| {
                let _ = write!(f, "{}", indent);
            });
            format_expression(f, first, indent, BranchPosition::Other)?;
        }
        Expression::Cast { data, typ } => {
            let mut indent = indent.clone();
            if pos.last() {
                indent.push(INDENT);
            } else {
                indent.push(INDENT_WITH_BRANCH);
            }
            writeln!(f, "Cast")?;
            indent.iter().for_each(|indent| {
                let _ = write!(f, "{}", indent);
            });
            writeln!(f, "├──Expression")?;
            let mut indent_data = indent.clone();
            indent_data.push(INDENT_WITH_BRANCH);
            format_expression(f, data, indent_data.clone(), BranchPosition::Last)?;
            indent.iter().for_each(|indent| {
                let _ = write!(f, "{}", indent);
            });
            writeln!(f, "└──Type")?;
            indent.iter().for_each(|indent| {
                let _ = write!(f, "{}", indent);
            });
            writeln!(f, "   └──{}", typ)?;
        }
        Expression::SubSelect(sub_select) => {
            writeln!(f, "SubSelect")?;
            let mut indent = indent.clone();
            indent.push(INDENT);
            format_statement(f, &Statement::SelectStatement(*sub_select.clone()), indent)?;
        }
        _ => {
            writeln!(f, "{:#?}", expr)?;
        }
    }
    Ok(())
}
