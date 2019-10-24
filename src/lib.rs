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

pub enum Info {
    NoInfo,
    FileInfo(String),
}

impl ToDoc for Info {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Info::NoInfo => Doc::text(""),
            Info::FileInfo(info) => {
                Doc::text(" @[")
                    .append(Doc::text(info))
                    .append(Doc::text("]"))
            }
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
            }
        }
    }
}

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

pub enum DefPort {
    Port(Info, String, Dir, Type),
}

impl ToDoc for DefPort {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            DefPort::Port(info, name, dir, tpe) => {
                dir.to_doc()
                    .append(Doc::text(" "))
                    .append(Doc::text(name))
                    .append(Doc::text(" "))
                    .append(Doc::text(":"))
                    .append(Doc::text(" "))
                    .append(tpe.to_doc())
                    .append(info.to_doc())
                    .append(Doc::newline())
            }
        }
    }
}

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

pub enum Stmt {
    EmptyStmt,
//    DefInstance(Info, String, String),
}

impl ToDoc for Stmt {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Stmt::EmptyStmt => Doc::text("skip"),
        }
    }
}

pub enum Param {
    IntParam(String, i64),
    StringParam(String, String),
    RawParam(String, String),
}

pub enum DefModule {
    Module(Info, String, Vec<DefPort>, Stmt),
    ExtModule(Info, String, Vec<DefPort>, String, Vec<Param>)
}

impl ToDoc for DefModule {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            DefModule::Module(info, name, _, stmt) => {
                Doc::text("module")
                    .append(Doc::text(" "))
                    .append(Doc::text(name))
                    .append(Doc::text(" :"))
                    .append(info.to_doc())
                    .append(Doc::newline())
                    .append(Doc::text("    "))
                    .append(stmt.to_doc())

            }
            DefModule::ExtModule(info, name, _, defname, _) => {
                Doc::text("extmodule")
                    .append(Doc::text(" "))
                    .append(Doc::text(name))
                    .append(Doc::text(" :"))
                    .append(info.to_doc())
                    .append(Doc::newline())
                    .append(Doc::text("  defname = "))
                    .append(Doc::text(defname))
            }
        }
    }
}

pub enum DefCircuit {
    Circuit(Info, Vec<DefModule>, String),
}

impl ToDoc for DefCircuit {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            DefCircuit::Circuit(info, modules, main) => {
                Doc::text("circuit ")
                    .append(Doc::text(main))
                    .append(Doc::text(" :"))
                    .append(info.to_doc())
                    .append(Doc::newline())
                    .append(Doc::text("  "))
                    .append(modules[0].to_doc())
                    .append(Doc::newline())
            }
        }
    }
}

pub fn verilog_compiler(input: &str, output: &str) {
    use std::io::{self, Write};
    use std::process::Command;
    use std::path::Path;
    let firrtl_bin = Path::new(".").join("firrtl/utils/bin/firrtl");
    let mut firrtl_args = Vec::new();
    firrtl_args.push("-i");
    firrtl_args.push(input);
    firrtl_args.push("-o");
    firrtl_args.push(output);
    firrtl_args.push("-X");
    firrtl_args.push("verilog");
    let firrtl_cmd = Command::new(&firrtl_bin)
                     .args(&firrtl_args)
                     .output()
                     .expect("failed to compile firrtl");
    io::stdout().write_all(&firrtl_cmd.stdout).unwrap();
    io::stderr().write_all(&firrtl_cmd.stderr).unwrap();
    assert!(firrtl_cmd.status.success(), "failed to compile firrtl");
}

pub fn emit(cir: DefCircuit, name: &str) {
    use std::fs::File;
    use std::io::{BufWriter, Write};
    let firrtl_name = format!("{}.fir", name);
    let v_name = format!("{}.v", name);
    let f = File::create(&firrtl_name).expect("Unable to create file");
    let mut buf = BufWriter::new(f);
    buf.write_all(cir.to_pretty().as_bytes()).expect("Unable to write data");
    buf.flush();
    verilog_compiler(&firrtl_name, &v_name);
}

#[cfg(test)]
mod tests{
    use super::*;
    use Info::*;
    use Type::*;
    use Expr::*;
    use PrimOp::*;
    use Dir::*;
    use DefPort::*;
    use Stmt::*;
    use DefModule::*;
    use DefCircuit::*;

    #[test]
    fn test_no_info() {
        assert_eq!(NoInfo.to_pretty(), "");
    }

    #[test]
    fn test_fileinfo() {
        assert_eq!(FileInfo("FooBar".into()).to_pretty(), " @[FooBar]");
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
        assert_eq!(DoPrim(Add, a, vec![], UInt(w)).to_pretty(), "add(a, b)");
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
        assert_eq!(Port(i, n, d, t).to_pretty(), "input in : UInt<32>\n");
    }

    #[test]
    fn test_port_output() {
        let i = NoInfo;
        let n = String::from("out");
        let d = Output;
        let t = Vector(Rc::new(UInt(32)), 8);
        assert_eq!(Port(i, n, d, t).to_pretty(), "output out : UInt<32>[8]\n");
    }

    #[test]
    fn test_stmt_empty() {
        assert_eq!(EmptyStmt.to_pretty(), "skip");
    }

    #[test]
    fn test_extmodule_empty() {
        let exp = "extmodule foo:\n  defname = bar";
        assert_eq!(ExtModule(NoInfo, "foo".into(), vec![], "bar".into(), vec![]).to_pretty(), exp);
    }

    #[test]
    fn test_circuit_empty() {
        assert_eq!(Circuit(NoInfo, vec![], "top".into()).to_pretty(), "circuit top :");
    }
}
