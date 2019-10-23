use std::rc::Rc;
use pretty::{Doc, BoxDoc};

pub type Info = String;

enum Dir {
    Input,
    Output
}

pub enum Type {
    Clock,
    Reset,
    UInt(u64),
    Vector(Rc<Type>, u64),
}

use Type::*;

impl Type {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Clock => Doc::text("Clock"),
            Reset => Doc::text("Reset"),
            UInt(width) => {
                Doc::concat(
                    vec![
                        Doc::text("UInt"),
                        Doc::text("<"),
                        Doc::as_string(width),
                        Doc::text(">")
                    ]
                )
            },
            Vector(ty, size) => {
                ty.to_doc()
                    .append(Doc::text("["))
                    .append(Doc::as_string(size))
                    .append(Doc::text("]"))
            },
        }
    }
    // given that firrtl has a spec format, we should
    // set the width accordingly? using 100 for now
    pub fn to_pretty(&self) -> String {
        let width: usize = 100;
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

#[cfg(test)]
mod test{
    use super::*;

    #[test]
    fn test_info() {
        assert_eq!(Info::from("hello"), "hello");
    }

    #[test]
    fn test_clock() {
        assert_eq!(Clock.to_pretty(), "Clock");
    }

    #[test]
    fn test_reset() {
        assert_eq!(Reset.to_pretty(), "Reset");
    }

    #[test]
    fn test_uint() {
        assert_eq!(UInt(3).to_pretty(), "UInt<3>");
    }

    #[test]
    fn test_vector() {
        assert_eq!(Vector(Rc::new(UInt(3)), 10).to_pretty(), "UInt<3>[10]");
    }
}