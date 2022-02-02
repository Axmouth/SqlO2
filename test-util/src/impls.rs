use crate::TestResultType;

pub trait TestSubjectExt {
    fn init() -> Self;
}

pub trait TestResultExt {
    fn result_type(&self) -> TestResultType;

    fn stringified(&self) -> String;
}

impl<T, E> TestResultExt for Result<T, E>
where
    T: TestStringify,
    E: TestStringify,
{
    fn result_type(&self) -> TestResultType {
        match self {
            Ok(_) => TestResultType::Ok,
            Err(_) => TestResultType::Err,
        }
    }

    fn stringified(&self) -> String {
        match self {
            Ok(t) => t.stringify(),
            Err(e) => e.stringify(),
        }
    }
}

impl<T> TestResultExt for Option<T>
where
    T: TestStringify,
{
    fn result_type(&self) -> TestResultType {
        match self {
            Some(_) => TestResultType::Some,
            None => TestResultType::None,
        }
    }

    fn stringified(&self) -> String {
        match self {
            Some(t) => t.stringify(),
            None => "".to_string(),
        }
    }
}

impl TestResultExt for String {
    fn result_type(&self) -> TestResultType {
        TestResultType::Unknown
    }

    fn stringified(&self) -> String {
        format!("{:#?}", self)
    }
}

pub trait TestStringify {
    fn stringify(&self) -> String;
}

impl TestStringify for String {
    fn stringify(&self) -> String {
        format!("{:#?}", self)
    }
}

impl TestStringify for &str {
    fn stringify(&self) -> String {
        format!("{:#?}", self)
    }
}
