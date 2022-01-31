extern crate test_macros;

fn main() {}

#[cfg(test)]
mod test_tests {
    use pretty_assertions::{assert_eq, assert_ne};
    use test_macros::test_case;

    #[test_case([1, 2, 4], [1, 2, 4])]
    #[test_case([2, 2, 3], [2, 2, 3])]
    #[test_case([1, 3, 3], [1, 3, 3])]
    #[test_case([1, 2, 3], [1, 2, 3])]
    fn eqs<T>(a: T, b: T)
    where
        T: Eq + std::fmt::Debug,
    {
        assert_eq!(a, b);
    }

    #[test_case([1, 2, 3], [2, 2, 3])]
    #[test_case([1, 2, 3], [2, 3, 1])]
    fn not_equal<T>(a: T, b: T)
    where
        T: Eq + std::fmt::Debug,
    {
        assert_ne!(a, b);
    }
}
