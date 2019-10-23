use std::rc::Rc;
use pretty::{Doc, BoxDoc};

pub trait ToDoc {
    fn to_doc(&self) -> Doc<BoxDoc<()>>;

    fn to_pretty_with_width(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn to_pretty(&self) -> String {
        self.to_pretty_with_width(100)
    }
}

pub type Id = String;

// We can probably just use
pub enum Info {
    NoInfo,
}

impl ToDoc for Info {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Info::NoInfo => Doc::text(""),
        }
    }

}

// pub enum Type {
//     UInt(Option<i64>),
//     SInt(Option<i64>),
//     // Fixed,
//     Clock,
//     Analog(Option<i64>),
//     Bundle(Vec<Field>),
//     Vector(Box<Type>, i64),
// }

pub enum Type {
    Clock,
    Reset,
    UInt(u64),
    Vector(Rc<Type>, u64),
}

impl ToDoc for Type {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
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
}

pub enum Expr {
    Reference(String, Type),
    SubField(Rc<Expr>, String, Type),
    SubIndex(Rc<Expr>, u64, Type),
    SubAccess(Rc<Expr>, Rc<Expr>, Type),
}

// pub enum Exp {
//     UInt(u64, u64),
//     UIntBits(u64, String),
//     Int(u64, i64),
//     IntBits(u64, String),
//     Reference(Id),
//     Subfield(Box<Expr>, Id),
//     Subindex(Box<Expr>, i64),
//     Subaccess(Box<Expr>, Box<Expr>),
//     Mux(Box<Expr>, Box<Expr>, Box<Expr>),
//     ValidIf(Box<Expr>, Box<Expr>),
//     Primitive(PrimOp)
// }

impl ToDoc for Expr {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Expr::Reference(name, _) => Doc::text(name),
            Expr::SubField(expr, name, _) => {
                expr.to_doc()
                    .append(Doc::text("."))
                    .append(Doc::text(name))
            },
            Expr::SubIndex(expr, value, _) => {
                expr.to_doc()
                    .append(Doc::text("["))
                    .append(Doc::as_string(value))
                    .append(Doc::text("]"))
            },
            Expr::SubAccess(e1, e2, _) => {
                e1.to_doc()
                    .append(Doc::text("["))
                    .append(e2.to_doc())
                    .append(Doc::text("]"))
            },
        }
    }
}

pub struct Circuit {
    pub id: Id,
    // info: Info,
    pub modules: Vec<Module>
}

pub struct Module {
    pub id: Id,
    // info: Info,
    pub ports: Vec<Port>,
    pub stmt: Vec<Stmt>,
}

pub enum Port {}

pub enum Dir {
    Input,
    Output
}


pub struct Field {
    flip: bool,
    id: Id,
    ty: Type,
}

pub enum Stmt {
    Wire(Id, Type, Info),
    Connect(Expr, Expr, Info),
    When(Expr, Box<Stmt>, Box<Stmt>),
    P
}

pub enum PrimOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    LEq,
    Gt,
    GEq,
    Eq,
    NEq,
    Pad,
    AsUInt,
    AsSInt,
    AsClock,
    Shl,
    Shr,
    Dshl,
    Dshr,
    Cvt,
    Neg,
    Not,
    And,
    Or,
    Xor,
    Andr,
    Orr,
    Xorr,
    Cat,
    Bits,
    Head,
    Tail
}

impl ToDoc for Circuit {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        let mut doc = Doc::as_string("circuit")
            .append(Doc::space())
            .append(Doc::text(&self.id))
            .append(Doc::space())
            .append(Doc::as_string(":")).group();

        for module in &self.modules {
            doc = doc.append(Doc::newline())
                     .nest(4)
                     .append(module.to_doc());
        }

        doc
    }
}

impl ToDoc for Module {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        Doc::text("module")
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
        let e = Rc::new(Reference("b".into(), UInt(64)));
        let n = String::from("n");
        let t = UInt(32);
        assert_eq!(SubField(e, n, t).to_pretty(), "b.n");
    }

    #[test]
    fn test_subindex() {
        let e = Rc::new(Reference("z".into(), UInt(43)));
        let v = 10;
        let t = UInt(32);
        assert_eq!(SubIndex(e, v, t).to_pretty(), "z[10]");
    }

    #[test]
    fn test_subaccess() {
        let e1 = Rc::new(Reference("in".into(), UInt(32)));
        let e2 = Rc::new(Reference("n".into(), UInt(5)));
        let t = UInt(32);
        assert_eq!(SubAccess(e1, e2, t).to_pretty(), "in[n]");
    }
}
