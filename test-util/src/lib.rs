use std::fmt::Debug;

use serde::Deserialize;

// Implement file based tests:
//
// A test is contained in a directory with the name of the test.
// A toml file defines the test query and the expected output combinations.
// The directory contains <name>.sql files with the sql input.
// The directory contains <name>.out files with the test function output stringified.
// The queries contain a key named result, with values err/ok/etc.
// The queries contain an key named filename, with the name used for output and input files.
// The queries contain an optional key named name, with the name of the test.
// The queries contain an optional key named description, with an extra explanation for that query/step.
// The queries contain an optional key named query, with the query string.
// Different types have a different result key, defined with a trait impl
// When the test finishes but the file does not match the result a <name>.test_diff is create in the same directory
// Proc macros can make this easier to write, by passing the folder path.
// The macro would execute the steps described in the test config(toml?) file in order, using the provided function.
// The macro would then use the output of said function to compare with the expected output.
// The macro would have to be able to use the same instance of the backend for the entire test.
// Maybe a type is passed to the macro, which it instantiates and passes to the function so it can be shared through the test.
// Macros for specific commonly used types could be prepared to make this easier.

// Consider ways of provisioning an initial database for acceptance tests to use.

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct TestQuerySection {
    pub name: Option<String>,
    pub description: Option<String>,
    pub query: Option<String>,
    pub result: TestResultType,
    pub filename: String,
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub struct TestConfig {
    #[serde(rename = "query")]
    pub queries: Vec<TestQuerySection>,
}

pub fn compare_output<T>(
    expected_file: &str,
    expected_result_type: &TestResultType,
    output: T,
) -> bool
where
    T: TestResultExt,
{
    let expected_output = match std::fs::read_to_string(expected_file) {
        Ok(s) => Some(s),
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                eprintln!("Expected output file {expected_file} not found");
                None
            } else {
                panic!(
                    "Error reading expected output file {}: {}",
                    expected_file, e
                );
            }
        }
    };

    let output_stringified = output.stringified();

    let is_expected_output = if let Some(expected_output) = expected_output {
        expected_output == output_stringified
    } else {
        false
    };

    let is_expected_result = expected_result_type == &output.result_type();

    let is_expected = is_expected_output && is_expected_result;

    if !is_expected_output {
        let diff_file = format!("{expected_file}.test_diff");
        eprintln!("Didn't get expected output, writing to {diff_file}",);
        std::fs::write(diff_file, output_stringified).expect("Unable to write file");
    }

    if !is_expected_result {
        eprintln!(
            "Didn't get expected result, expected {expected_result_type}, got {}",
            output.result_type()
        );
    }

    is_expected
}

pub fn run_test<T, O, F>(test: &str, test_fn: F, mut subject: T)
where
    T: TestSubjectExt,
    O: TestResultExt,
    F: Fn(&str, &mut T) -> O,
{
    let toml_file = format!("{test}/test.toml");
    let toml_str = std::fs::read_to_string(&toml_file)
        .unwrap_or_else(|_| panic!("Unable to read toml file {toml_file}"));
    let test_config: TestConfig = toml::from_str(&toml_str).expect("Failed to parse toml");

    let mut success = true;
    for (i, query) in test_config.queries.iter().enumerate() {
        let input_file = format!("{test}/{}.sql", query.filename);
        let input = std::fs::read_to_string(&input_file)
            .unwrap_or_else(|_| panic!("Unable to input file {input_file}"));
        let result = test_fn(&input, &mut subject);
        let expected_file = format!("{test}/{}", query.filename);
        let is_expected = compare_output(&expected_file, &query.result, result);

        eprint!("Test step {i}");
        if let Some(name) = &query.name {
            eprint!(": {name}");
        }
        if let Some(description) = &query.description {
            eprint!("({description})");
        }
        eprintln!();

        if !is_expected {
            eprintln!("Test step {i} failed");
        }

        success = success && is_expected;
    }

    if !success {
        panic!("Test failed");
    }
}

#[derive(Debug, Deserialize, PartialEq, Eq)]
pub enum TestResultType {
    #[serde(rename = "ok")]
    Ok,
    #[serde(rename = "err")]
    Err,
    #[serde(rename = "some")]
    Some,
    #[serde(rename = "none")]
    None,
    #[serde(rename = "unknown")]
    Unknown,
}

impl std::fmt::Display for TestResultType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestResultType::Ok => write!(f, "ok"),
            TestResultType::Err => write!(f, "err"),
            TestResultType::Some => write!(f, "some"),
            TestResultType::None => write!(f, "none"),
            TestResultType::Unknown => write!(f, "unknown"),
        }
    }
}

pub trait TestSubjectExt {
    fn init() -> Self;
}

pub trait TestResultExt {
    fn result_type(&self) -> TestResultType;

    fn stringified(&self) -> String;
}

impl<T, E> TestResultExt for Result<T, E>
where
    T: Debug,
    E: Debug,
{
    fn result_type(&self) -> TestResultType {
        match self {
            Ok(_) => TestResultType::Ok,
            Err(_) => TestResultType::Err,
        }
    }

    fn stringified(&self) -> String {
        match self {
            Ok(t) => format!("{:#?}", t),
            Err(e) => format!("{:#?}", e),
        }
    }
}

impl<T> TestResultExt for Option<T>
where
    T: Debug,
{
    fn result_type(&self) -> TestResultType {
        match self {
            Some(_) => TestResultType::Some,
            None => TestResultType::None,
        }
    }

    fn stringified(&self) -> String {
        match self {
            Some(t) => format!("{:#?}", t),
            None => "".to_string(),
        }
    }
}

impl TestResultExt for String {
    fn result_type(&self) -> TestResultType {
        TestResultType::Unknown
    }

    fn stringified(&self) -> String {
        self.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn read_toml() {
        let toml_str = std::fs::read_to_string("../tests/integration/test1/test.toml")
            .expect("Unable to toml file");
        let decoded: TestConfig = toml::from_str(&toml_str).expect("Failed to parse toml");

        let expected = TestConfig {
            queries: vec![
                TestQuerySection {
                    name: None,
                    description: None,
                    query: None,
                    result: TestResultType::Err,
                    filename: "query1".to_string(),
                },
                TestQuerySection {
                    name: None,
                    description: None,
                    query: None,
                    result: TestResultType::Ok,
                    filename: "query2".to_string(),
                },
                TestQuerySection {
                    name: Some("query3 name".to_string()),
                    description: Some("query3 description".to_string()),
                    query: Some("query3 query".to_string()),
                    result: TestResultType::Ok,
                    filename: "query2".to_string(),
                },
            ],
        };

        assert_eq!(expected, decoded);
    }
}
