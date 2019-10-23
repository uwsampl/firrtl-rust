pub fn emit() -> String { String::from("hello") }

#[cfg(test)]
mod test{
    use super::*;

    #[test]
    fn test_emit() {
        assert_eq!("hello", emit());
    }
}