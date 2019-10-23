use std::rc::Rc;
use pretty::{Doc, BoxDoc};

pub enum Info {
    NoInfo,
}

impl Info {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Info::NoInfo => Doc::text(""),
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

pub enum Type {
    Clock,
    Reset,
    UInt(u64),
    Vector(Rc<Type>, u64),
}

impl Type {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Type::Clock => Doc::text("Clock"),
            Type::Reset => Doc::text("Reset"),
            Type::UInt(width) => {
                Doc::concat(
                    vec![
                        Doc::text("UInt"),
                        Doc::text("<"),
                        Doc::as_string(width),
                        Doc::text(">")
                    ]
                )
            },
            Type::Vector(ty, size) => {
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

enum Expr {
    Reference(String, Type),
    SubField(Rc<Expr>, String, Type),
    // SubIndex(Rc<Expr>, u64, Type),
    // SubAccess(Rc<Expr>, Rc<Expr>, Type),
}

impl Expr {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Expr::Reference(name, _) => Doc::text(name),
            Expr::SubField(expr, name, _) => {
                expr.to_doc()
                    .append(Doc::text("."))
                    .append(Doc::text(name))
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
    use Info::*;
    use Type::*;
    use Expr::*;

    #[test]
    fn test_no_info() {
        assert_eq!(NoInfo.to_pretty(), "");
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
        let w = 3;
        assert_eq!(UInt(w).to_pretty(), "UInt<3>");
    }

    #[test]
    fn test_vector() {
        let t = Rc::new(UInt(3));
        let s = 10;
        assert_eq!(Vector(t, s).to_pretty(), "UInt<3>[10]");
    }

    #[test]
    fn test_reference() {
        let n = String::from("foo");
        let t = UInt(64);
        assert_eq!(Reference(n, t).to_pretty(), "foo");
    }

    #[test]
    fn test_subfield() {
        let r = Rc::new(Reference("b".into(), UInt(64)));
        let n = String::from("n");
        let t = UInt(32);
        assert_eq!(SubField(r, n, t).to_pretty(), "b.n");
    }
}