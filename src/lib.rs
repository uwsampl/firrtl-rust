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
                Doc::space()
                    .append(Doc::text("@["))
                    .append(Doc::text(info))
                    .append(Doc::text("]")).group()
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
                    .append(Doc::space())
                    .append(Doc::text(name))
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(Doc::space())
                    .append(tpe.to_doc())
                    .append(info.to_doc())
                    .append(Doc::newline()).group()
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
    DefInstance(Info, String, String),
}

impl ToDoc for Stmt {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Stmt::EmptyStmt => Doc::text("skip"),
            Stmt::DefInstance(info, name, module) => {
                Doc::text("inst")
                    .append(Doc::space())
                    .append(Doc::text(name))
                    .append(Doc::space())
                    .append(Doc::text("of"))
                    .append(Doc::space())
                    .append(Doc::text(module))
                    .append(info.to_doc()).group()
            },
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
            DefModule::Module(info, name, ports, stmt) => {
                let mut doc = Doc::text("module")
                    .append(Doc::space())
                    .append(Doc::text(name))
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(info.to_doc())
                    .append(Doc::newline()).group();
                for p in ports {
                    doc = doc.append(p.to_doc());
                }
                doc = doc.nest(4)
                    .append(stmt.to_doc());
                doc
            }
            DefModule::ExtModule(info, name, ports, defname, _) => {
                let mut doc = Doc::text("extmodule")
                    .append(Doc::space())
                    .append(Doc::text(name))
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(info.to_doc())
                    .append(Doc::newline()).group();
                for p in ports {
                    doc = doc.append(p.to_doc());
                }
                    doc = doc.nest(2)
                    .append(Doc::text("defname"))
                    .append(Doc::space())
                    .append(Doc::text("="))
                    .append(Doc::space())
                    .append(Doc::text(defname)).group();
                doc
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
                let mut doc = Doc::text("circuit")
                    .append(Doc::space())
                    .append(Doc::text(main))
                    .append(Doc::space())
                    .append(Doc::text(":"))
                    .append(info.to_doc()).group();
                for m in modules {
                    doc = doc.append(Doc::newline())
                        .nest(2)
                        .append(m.to_doc())
                        .append(Doc::newline());
                }
                doc
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

pub fn read_verilog(name: &str) -> String {
    use std::fs;
    let s = fs::read_to_string(format!("{}.v", name))
        .expect("Something went wrong reading the file");
    s
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
        let expect = "";
        assert_eq!(NoInfo.to_pretty(), expect);
    }

    #[test]
    fn test_fileinfo() {
        let info = "FooBar";
        let expect = format!(" @[{}]", info);
        assert_eq!(FileInfo(info.into()).to_pretty(), expect);
    }

    #[test]
    fn test_clock() {
        let expect = "Clock";
        assert_eq!(Clock.to_pretty(), expect);
    }

    #[test]
    fn test_reset() {
        let expect = "Reset";
        assert_eq!(Reset.to_pretty(), expect);
    }

    #[test]
    fn test_uint() {
        let w = 3;
        let expect = format!("UInt<{}>", w);
        assert_eq!(UInt(w).to_pretty(), expect);
    }

    #[test]
    fn test_vector() {
        let w = 32;
        let t = Rc::new(UInt(w));
        let s = 10;
        let expect = format!("UInt<{}>[{}]", w, s);
        assert_eq!(Vector(t, s).to_pretty(), expect);
    }

    #[test]
    fn test_reference() {
        let w = 64;
        let n = "foo";
        let t = UInt(w);
        let expect = format!("{}", n);
        assert_eq!(Reference(n.into(), t).to_pretty(), expect);
    }

    #[test]
    fn test_subfield() {
        let i = "b";
        let f = "n";
        let w = 32;
        let t = UInt(w);
        let y = 64;
        let u = UInt(y);
        let expr = Rc::new(Reference(i.into(), t));
        let expect = format!("{}.{}", i, f);
        assert_eq!(SubField(expr, f.into(), u).to_pretty(), expect);
    }

    #[test]
    fn test_subindex() {
        let i = "z";
        let a = 10;
        let w = 32;
        let x = 64;
        let t = UInt(w);
        let u = UInt(x);
        let expect = format!("{}[{}]", i, a);
        let expr = Rc::new(Reference(i.into(), t));
        assert_eq!(SubIndex(expr, a, u).to_pretty(), expect);
    }

    #[test]
    fn test_subaccess() {
        let p = "in";
        let i = "n";
        let w = 32;
        let x = 64;
        let y = 8;
        let t = UInt(w);
        let u = UInt(x);
        let v = UInt(y);
        let expect = format!("{}[{}]", p, i);
        let expr1 = Rc::new(Reference(p.into(), t));
        let expr2 = Rc::new(Reference(i.into(), u));
        assert_eq!(SubAccess(expr1, expr2, v).to_pretty(), expect);
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
        let op1 = "a";
        let op2 = "b";
        let w = 32;
        let expr1 = Reference(op1.into(), UInt(w));
        let expr2 = Reference(op2.into(), UInt(w));
        let expr = vec![expr1, expr2];
        let expect = format!("add({}, {})", op1, op2);
        assert_eq!(DoPrim(Add, expr, vec![], UInt(w)).to_pretty(), expect);
    }

    #[test]
    fn test_dir_input() {
        let expect = format!("{}", "input");
        assert_eq!(Input.to_pretty(), expect);
    }

    #[test]
    fn test_dir_output() {
        let expect = format!("{}", "output");
        assert_eq!(Output.to_pretty(), expect);
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
    fn test_stmt_instance() {
        let i = "a0";
        let m = "adder";
        let e = format!("inst {} of {}", i, m);
        assert_eq!(DefInstance(NoInfo, i.into(), m.into()).to_pretty(), e);
    }

    #[test]
    fn test_extmodule_empty() {
        let e = "extmodule foo :\n  defname = bar";
        assert_eq!(ExtModule(NoInfo, "foo".into(), vec![], "bar".into(), vec![]).to_pretty(), e);
    }

    #[test]
    fn test_circuit_empty() {
        assert_eq!(Circuit(NoInfo, vec![], "top".into()).to_pretty(), "circuit top :");
    }
}
