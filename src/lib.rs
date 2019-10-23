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
    FileInfo(String),
}

impl ToDoc for Info {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Info::NoInfo => Doc::text(""),
            Info::FileInfo(info) => Doc::text(info),
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
    DoPrim(PrimOp, Vec<Expr>, Vec<u64>, Type),
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
            Expr::DoPrim(op, args, consts, _) => {
                op.to_doc()
                    .append(Doc::text("("))
                    .append(Doc::intersperse(
                        args.iter().map(|i| i.to_doc()),
                        Doc::text(", ")))
                    .append(Doc::intersperse(
                        consts.iter().map(|i| Doc::as_string(i)),
                        Doc::text(", ")))
                    .append(Doc::text(")"))
                    .append(Doc::space())
            }
        }
    }
}

// pub struct Circuit {
//     pub id: Id,
//     // info: Info,
//     pub modules: Vec<Module>
// }

// pub struct Module {
//     pub id: Id,
//     // info: Info,
//     pub ports: Vec<IO>,
//     pub stmt: Vec<Stmt>,
// }

pub enum Dir {
    Input,
    Output
}

impl ToDoc for Dir {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Dir::Input => Doc::text("input"),
            Dir::Output => Doc::text("output"),
        }
    }
}

pub enum IO {
    Port(Info, String, Dir, Type),
}

impl ToDoc for IO {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            IO::Port(info, name, dir, tpe) => {
                dir.to_doc()
                    .append(Doc::text(" "))
                    .append(Doc::text(name))
                    .append(Doc::text(" "))
                    .append(Doc::text(":"))
                    .append(Doc::text(" "))
                    .append(tpe.to_doc())
                    .append(info.to_doc())
                    .append(Doc::space())
            }
        }
    }
}

// pub struct Field {
//     flip: bool,
//     id: Id,
//     ty: Type,
// }

// pub enum Stmt {
//     Wire(Id, Type, Info),
//     Connect(Expr, Expr, Info),
//     When(Expr, Box<Stmt>, Box<Stmt>),
//     P
// }

pub enum PrimOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    Pad,
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

impl ToDoc for PrimOp {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            PrimOp::Add => Doc::text("add"),
            PrimOp::Sub => Doc::text("sub"),
            PrimOp::Mul => Doc::text("mul"),
            PrimOp::Div => Doc::text("div"),
            PrimOp::Rem => Doc::text("rem"),
            PrimOp::Lt => Doc::text("lt"),
            PrimOp::Leq => Doc::text("leq"),
            PrimOp::Gt => Doc::text("gt"),
            PrimOp::Geq => Doc::text("geq"),
            PrimOp::Eq => Doc::text("eq"),
            PrimOp::Neq => Doc::text("neq"),
            PrimOp::Pad => Doc::text("pad"),
            PrimOp::Shl => Doc::text("shl"),
            PrimOp::Shr => Doc::text("shr"),
            PrimOp::Dshl => Doc::text("dshl"),
            PrimOp::Dshr => Doc::text("dshr"),
            PrimOp::Cvt => Doc::text("cvt"),
            PrimOp::Neg => Doc::text("neg"),
            PrimOp::Not => Doc::text("not"),
            PrimOp::And => Doc::text("and"),
            PrimOp::Or => Doc::text("or"),
            PrimOp::Xor => Doc::text("xor"),
            PrimOp::Andr => Doc::text("andr"),
            PrimOp::Orr => Doc::text("orr"),
            PrimOp::Xorr => Doc::text("xorr"),
            PrimOp::Cat => Doc::text("cat"),
            PrimOp::Bits => Doc::text("bits"),
            PrimOp::Head => Doc::text("head"),
            PrimOp::Tail => Doc::text("tail"),
        }
    }
}

// impl ToDoc for Circuit {
//     fn to_doc(&self) -> Doc<BoxDoc<()>> {
//         let mut doc = Doc::as_string("circuit")
//             .append(Doc::space())
//             .append(Doc::text(&self.id))
//             .append(Doc::space())
//             .append(Doc::as_string(":")).group();

//         for module in &self.modules {
//             doc = doc.append(Doc::newline())
//                      .nest(4)
//                      .append(module.to_doc());
//         }

//         doc
//     }
// }

// impl ToDoc for Module {
//     fn to_doc(&self) -> Doc<BoxDoc<()>> {
//         Doc::text("module")
//     }
// }

#[cfg(test)]
mod test{
    use super::*;
    use Info::*;
    use Type::*;
    use Expr::*;
    use PrimOp::*;
    use Dir::*;
    use IO::*;

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

    fn _test_primops(op: PrimOp, s: &str) {
        assert_eq!(op.to_pretty(), s);
    }

    #[test]
    fn test_primops() {
        _test_primops(Add, "add");
        _test_primops(Add, "add");
        _test_primops(Mul, "mul");
        _test_primops(Div, "div");
        _test_primops(Rem, "rem");
        _test_primops(Lt, "lt");
        _test_primops(Leq, "leq");
        _test_primops(Gt, "gt");
        _test_primops(Geq, "geq");
        _test_primops(Eq, "eq");
        _test_primops(Neq, "neq");
        _test_primops(Pad, "pad");
        _test_primops(Shl, "shl");
        _test_primops(Shr, "shr");
        _test_primops(Dshl, "dshl");
        _test_primops(Dshr, "dshr");
        _test_primops(Cvt, "cvt");
        _test_primops(Neg, "neg");
        _test_primops(Not, "not");
        _test_primops(And, "and");
        _test_primops(Or, "or");
        _test_primops(Xor, "xor");
        _test_primops(Andr, "andr");
        _test_primops(Orr, "orr");
        _test_primops(Xorr, "xorr");
        _test_primops(Cat, "cat");
        _test_primops(Bits, "bits");
        _test_primops(Head, "head");
        _test_primops(Tail, "tail");
    }

    #[test]
    fn test_add() {
        let w = 32;
        let e1 = Reference("a".into(), UInt(w));
        let e2 = Reference("b".into(), UInt(w));
        let a = vec![e1, e2];
        assert_eq!(DoPrim(Add, a, vec![], UInt(w)).to_pretty(), "add(a, b)\n");
    }

    #[test]
    fn test_input() {
        assert_eq!(Input.to_pretty(), "input");
    }

    #[test]
    fn test_output() {
        assert_eq!(Output.to_pretty(), "output");
    }

    #[test]
    fn test_port_input() {
        let i = NoInfo;
        let n = String::from("in");
        let d = Input;
        let t = UInt(32);
        assert_eq!(IO::Port(i, n, d, t).to_pretty(), "input in : UInt<32>\n");
    }

    #[test]
    fn test_port_output() {
        let i = NoInfo;
        let n = String::from("out");
        let d = Output;
        let t = Vector(Rc::new(UInt(32)), 8);
        assert_eq!(IO::Port(i, n, d, t).to_pretty(), "output out : UInt<32>[8]\n");
    }
}
