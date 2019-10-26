use std::rc::Rc;
use pretty::{Doc, BoxDoc};
use std::process::Command;
use std::path::PathBuf;
use std::fs::File;
use std::io::{self, BufWriter, Write};

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

pub enum Width {
    UnknownWidth,
    IntWidth(u64),
}

impl ToDoc for Width {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Width::UnknownWidth => Doc::text(""),
            Width::IntWidth(width) => {
                Doc::text("<")
                    .append(Doc::as_string(width))
                    .append(Doc::text(">"))
            },
        }
    }
}

pub enum Type {
    Clock,
    Reset,
    UnknownType,
    UInt(Width),
    SInt(Width),
    Fixed(Width, Width),
    Vector(Rc<Type>, u64),
}

impl ToDoc for Type {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Type::Clock => Doc::text("Clock"),
            Type::Reset => Doc::text("Reset"),
            Type::UnknownType => Doc::text("?"),
            Type::UInt(width) => {
                Doc::text("UInt").append(width.to_doc())
            },
            Type::SInt(width) => {
                Doc::text("SInt").append(width.to_doc())
            },
            Type::Fixed(width, point) => {
                Doc::text("Fixed")
                    .append(width.to_doc())
                    .append(point.to_doc())
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
    DefNode(Info, String, Expr),
    Block(Vec<Stmt>),
    Connect(Info, Expr, Expr),
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
            Stmt::DefNode(info, name, expr) => {
                Doc::text("node")
                    .append(Doc::space())
                    .append(Doc::text(name))
                    .append(Doc::space())
                    .append(Doc::text("="))
                    .append(Doc::space())
                    .append(expr.to_doc())
                    .append(info.to_doc()).group()
            },
            Stmt::Block(stmts) => {
                let mut doc = Doc::text("");
                for s in stmts {
                    doc = doc.append(s.to_doc())
                        .append(Doc::newline());
                }
                doc
            },
            Stmt::Connect(info, loc, expr) => {
                Doc::text("")
                    .append(loc.to_doc())
                    .append(Doc::space())
                    .append(Doc::text("<="))
                    .append(Doc::space())
                    .append(expr.to_doc())
                    .append(info.to_doc()).group()
            }
        }
    }
}

pub enum Param {
    IntParam(String, i64),
    StringParam(String, String),
}

impl ToDoc for Param {
    fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match self {
            Param::IntParam(name, value) => {
                Doc::text("parameter")
                    .append(Doc::space())
                    .append(Doc::text(name))
                    .append(Doc::space())
                    .append(Doc::text("="))
                    .append(Doc::space())
                    .append(Doc::as_string(value)).group()
            },
            Param::StringParam(name, value) => {
                Doc::text("parameter")
                    .append(Doc::space())
                    .append(Doc::text(name))
                    .append(Doc::space())
                    .append(Doc::text("="))
                    .append(Doc::space())
                    .append(Doc::text(value)).group()
            },
        }
    }
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
                doc = doc.append(stmt.to_doc())
                    .nest(2).group();
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
                    doc = doc.append(Doc::text("defname"))
                        .append(Doc::space())
                        .append(Doc::text("="))
                        .append(Doc::space())
                        .append(Doc::text(defname))
                        .nest(2).group();
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
                        .append(m.to_doc())
                        .append(Doc::newline());
                }
                doc.nest(2).group()
            }
        }
    }
}

pub fn firrtl_compiler(fir: &str, verilog: &str) {
    let mut firrtl_bin = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    firrtl_bin.push("firrtl/utils/bin/firrtl");
    let mut firrtl_args = Vec::new();
    firrtl_args.push("-i");
    firrtl_args.push(fir);
    firrtl_args.push("-o");
    firrtl_args.push(verilog);
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

pub fn emit_firrtl(cir: DefCircuit, path: &str) -> std::io::Result<()>  {
    let mut buffer = BufWriter::new(File::create(path)?);
    buffer.write_all(cir.to_pretty().as_bytes())?;
    buffer.flush()?;
    Ok(())
}

pub fn emit_verilog(cir: DefCircuit, path: &str) {
    let fpath = format!("{}.fir", path);
    let vpath = format!("{}.v", path);
    emit_firrtl(cir, &fpath);
    firrtl_compiler(&fpath, &vpath);
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
    use Width::*;
    use Type::*;
    use Expr::*;
    use PrimOp::*;
    use Dir::*;
    use Stmt::*;
    use Param::*;
    use DefPort::*;
    use DefModule::*;
    use DefCircuit::*;

    #[test]
    fn test_info_noinfo() {
        let expect = format!("");
        assert_eq!(NoInfo.to_pretty(), expect);
    }

    #[test]
    fn test_info_fileinfo() {
        let info = "FooBar";
        let expect = format!(" @[{}]", info);
        assert_eq!(FileInfo(info.to_string()).to_pretty(), expect);
    }

    #[test]
    fn test_unknown_width() {
        let expect = format!("");
        assert_eq!(UnknownWidth.to_pretty(), expect);
    }

    #[test]
    fn test_int_width() {
        let w = 43;
        let expect = format!("<{}>", w);
        assert_eq!(IntWidth(w).to_pretty(), expect);
    }

    #[test]
    fn test_type_clock() {
        let expect = "Clock";
        assert_eq!(Clock.to_pretty(), expect);
    }

    #[test]
    fn test_type_reset() {
        let expect = "Reset";
        assert_eq!(Reset.to_pretty(), expect);
    }

    #[test]
    fn test_type_unknown() {
        let expect = "?";
        assert_eq!(UnknownType.to_pretty(), expect);
    }

    #[test]
    fn test_type_uint() {
        let w = 3;
        let expect = format!("UInt<{}>", w);
        assert_eq!(UInt(IntWidth(w)).to_pretty(), expect);
    }

    #[test]
    fn test_type_sint() {
        let w = 32;
        let expect = format!("SInt<{}>", w);
        assert_eq!(SInt(IntWidth(w)).to_pretty(), expect);
    }

    #[test]
    fn test_type_fixed_unknown_width() {
        let w = 32;
        let expect = format!("Fixed<{}>", w);
        assert_eq!(Fixed(IntWidth(w), UnknownWidth).to_pretty(), expect);
    }

    #[test]
    fn test_type_fixed() {
        let w = 32;
        let x = 3;
        let expect = format!("Fixed<{}><{}>", w, x);
        assert_eq!(Fixed(IntWidth(w), IntWidth(x)).to_pretty(), expect);
    }

    #[test]
    fn test_type_vector() {
        let s = 10;
        let w = 32;
        let t = UInt(IntWidth(w));
        let expect = format!("UInt<{}>[{}]", w, s);
        assert_eq!(Vector(Rc::new(t), s).to_pretty(), expect);
    }

    #[test]
    fn test_expr_reference() {
        let n = "foo";
        let w = 64;
        let t = UInt(IntWidth(w));
        let expect = format!("{}", n);
        assert_eq!(Reference(n.to_string(), t).to_pretty(), expect);
    }

    #[test]
    fn test_expr_subfield() {
        let i = "b";
        let f = "n";
        let w = 32;
        let t = UnknownType;
        let u = UnknownType;
        let expr = Rc::new(Reference(i.to_string(), t));
        let expect = format!("{}.{}", i, f);
        assert_eq!(SubField(expr, f.to_string(), u).to_pretty(), expect);
    }

    #[test]
    fn test_expr_subindex() {
        let i = "z";
        let a = 10;
        let t = UnknownType;
        let u = UnknownType;
        let expect = format!("{}[{}]", i, a);
        let expr = Rc::new(Reference(i.to_string(), t));
        assert_eq!(SubIndex(expr, a, u).to_pretty(), expect);
    }

    #[test]
    fn test_expr_subaccess() {
        let p = "in";
        let i = "n";
        let t = UnknownType;
        let u = UnknownType;
        let v = UnknownType;
        let expect = format!("{}[{}]", p, i);
        let expr1 = Rc::new(Reference(p.to_string(), t));
        let expr2 = Rc::new(Reference(i.to_string(), u));
        assert_eq!(SubAccess(expr1, expr2, v).to_pretty(), expect);
    }

    fn test_primops(op: PrimOp, expect: &str) {
        assert_eq!(op.to_pretty(), expect);
    }

    #[test]
    fn test_primops_add() {
        test_primops(Add, "add");
    }

    #[test]
    fn test_primops_sub() {
        test_primops(Sub, "sub");
    }

    #[test]
    fn test_primops_mul() {
        test_primops(Mul, "mul");
    }

    #[test]
    fn test_primops_div() {
        test_primops(Div, "div");
    }

    #[test]
    fn test_primops_rem() {
        test_primops(Rem, "rem");
    }

    #[test]
    fn test_primops_lt() {
        test_primops(Lt, "lt");
    }

    #[test]
    fn test_primops_leq() {
        test_primops(Leq, "leq");
    }

    #[test]
    fn test_primops_gt() {
        test_primops(Gt, "gt");
    }

    #[test]
    fn test_primops_geq() {
        test_primops(Geq, "geq");
    }

    #[test]
    fn test_primops_eq() {
        test_primops(Eq, "eq");
    }

    #[test]
    fn test_primops_neq() {
        test_primops(Neq, "neq");
    }

    #[test]
    fn test_primops_pad() {
        test_primops(Pad, "pad");
    }

    #[test]
    fn test_primops_shl() {
        test_primops(Shl, "shl");
    }

    #[test]
    fn test_primops_shr() {
        test_primops(Shr, "shr");
    }

    #[test]
    fn test_primops_dshl() {
        test_primops(Dshl, "dshl");
    }

    #[test]
    fn test_primops_dshr() {
        test_primops(Dshr, "dshr");
    }

    #[test]
    fn test_primops_cvt() {
        test_primops(Cvt, "cvt");
    }

    #[test]
    fn test_primops_neg() {
        test_primops(Neg, "neg");
    }

    #[test]
    fn test_primops_not() {
        test_primops(Not, "not");
    }

    #[test]
    fn test_primops_and() {
        test_primops(And, "and");
    }

    #[test]
    fn test_primops_or() {
        test_primops(Or, "or");
    }

    #[test]
    fn test_primops_xor() {
        test_primops(Xor, "xor");
    }

    #[test]
    fn test_primops_andr() {
        test_primops(Andr, "andr");
    }

    #[test]
    fn test_primops_orr() {
        test_primops(Orr, "orr");
    }

    #[test]
    fn test_primops_xorr() {
        test_primops(Xorr, "xorr");
    }

    #[test]
    fn test_primops_cat() {
        test_primops(Cat, "cat");
    }

    #[test]
    fn test_primops_bits() {
        test_primops(Bits, "bits");
    }

    #[test]
    fn test_primops_head() {
        test_primops(Head, "head");
    }

    #[test]
    fn test_primops_tail() {
        test_primops(Tail, "tail");
    }

    #[test]
    fn test_expr_primops_add() {
        let op1 = "a";
        let op2 = "b";
        let w = 32;
        let expr1 = Reference(op1.to_string(), UInt(IntWidth(w)));
        let expr2 = Reference(op2.to_string(), UInt(IntWidth(w)));
        let expr = vec![expr1, expr2];
        let expect = format!("add({}, {})", op1, op2);
        assert_eq!(DoPrim(Add, expr, vec![], UInt(IntWidth(w))).to_pretty(), expect);
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
        let n = "in";
        let d = Input;
        let w = 32;
        let t = UInt(IntWidth(w));
        let expect = format!("input {} : UInt<{}>\n", n, w);
        assert_eq!(Port(i, n.to_string(), d, t).to_pretty(), expect);
    }

    #[test]
    fn test_port_output() {
        let i = NoInfo;
        let n = "out";
        let d = Output;
        let w = 32;
        let t = UInt(IntWidth(w));
        let expect = format!("output {} : UInt<{}>\n", n, w);
        assert_eq!(Port(i, n.to_string(), d, t).to_pretty(), expect);
    }

    #[test]
    fn test_stmt_empty() {
        let expect = format!("{}", "skip");
        assert_eq!(EmptyStmt.to_pretty(), expect);
    }

    #[test]
    fn test_stmt_node() {
        let n = "n0";
        let r = "a";
        let expr = Reference(r.to_string(), UInt(IntWidth(32)));
        let expect = format!("node {} = {}", n, r);
        assert_eq!(DefNode(NoInfo, n.to_string(), expr).to_pretty(), expect);
    }

    #[test]
    fn test_stmt_instance() {
        let i = "a0";
        let m = "adder";
        let expect = format!("inst {} of {}", i, m);
        assert_eq!(DefInstance(NoInfo, i.to_string(), m.to_string()).to_pretty(), expect);
    }

    #[test]
    fn test_stmt_block() {
        let stmts = vec![EmptyStmt, EmptyStmt];
        let expect = format!("{}\n{}\n", "skip", "skip");
        assert_eq!(Block(stmts).to_pretty(), expect);
    }

    #[test]
    fn test_stmt_connect() {
        let op1 = "a";
        let op2 = "b";
        let expr1 = Reference(op1.to_string(), UnknownType);
        let expr2 = Reference(op2.to_string(), UnknownType);
        let expect = format!("{} <= {}", op1, op2);
        assert_eq!(Connect(NoInfo, expr1, expr2).to_pretty(), expect);
    }

    #[test]
    fn test_param_int() {
        let name = "WIDTH";
        let val = 3;
        let expect = format!("parameter {} = {}", name, val);
        assert_eq!(IntParam(name.to_string(), val).to_pretty(), expect);
    }

    #[test]
    fn test_param_string() {
        let name = "ADDR";
        let val = "{32'h00, 32'h01}";
        let expect = format!("parameter {} = {}", name, val);
        assert_eq!(StringParam(name.to_string(), val.to_string()).to_pretty(), expect);
    }

    #[test]
    fn test_defmodule_extmodule() {
        let n = "foo";
        let d = "bar";
        let expect = format!("extmodule {} :\n  defname = {}", n, d);
        assert_eq!(ExtModule(NoInfo, n.to_string(), vec![], d.to_string(), vec![]).to_pretty(), expect);
    }

    #[test]
    fn test_defmodule_module() {
        let n = "foo";
        let expect = format!("module {} :\n  skip", n);
        assert_eq!(Module(NoInfo, n.to_string(), vec![], EmptyStmt).to_pretty(), expect);
    }

    #[test]
    fn test_circuit() {
        let n = "top";
        let expect = format!("circuit {} :", n);
        assert_eq!(Circuit(NoInfo, vec![], n.to_string()).to_pretty(), expect);
    }
}
