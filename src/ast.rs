use std::rc::Rc;

pub trait Doc {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>>;

    fn pretty_with_width(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn pretty(&self) -> String {
        self.pretty_with_width(100)
    }
}

#[derive(Clone, Debug)]
pub enum Info {
    NoInfo,
    FileInfo(String),
}

impl Doc for Info {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            Info::NoInfo => pretty::Doc::text(""),
            Info::FileInfo(info) => {
                pretty::Doc::space()
                    .append(pretty::Doc::text("@["))
                    .append(pretty::Doc::text(info))
                    .append(pretty::Doc::text("]")).group()
            }
        }
    }

}

#[derive(Clone, Debug)]
pub enum Width {
    UnknownWidth,
    IntWidth(u64),
}

impl Doc for Width {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            Width::UnknownWidth => pretty::Doc::text(""),
            Width::IntWidth(width) => {
                pretty::Doc::text("<")
                    .append(pretty::Doc::as_string(width))
                    .append(pretty::Doc::text(">"))
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Clock,
    Reset,
    UnknownType,
    UInt(Width),
    SInt(Width),
    Fixed(Width, Width),
    Vector(Rc<Type>, u64),
}

impl Doc for Type {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            Type::Clock => pretty::Doc::text("Clock"),
            Type::Reset => pretty::Doc::text("Reset"),
            Type::UnknownType => pretty::Doc::text("?"),
            Type::UInt(width) => {
                pretty::Doc::text("UInt").append(width.doc())
            },
            Type::SInt(width) => {
                pretty::Doc::text("SInt").append(width.doc())
            },
            Type::Fixed(width, point) => {
                pretty::Doc::text("Fixed")
                    .append(width.doc())
                    .append(point.doc())
            },
            Type::Vector(ty, size) => {
                ty.doc()
                    .append(pretty::Doc::text("["))
                    .append(pretty::Doc::as_string(size))
                    .append(pretty::Doc::text("]"))
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Reference(String, Type),
    SubField(Rc<Expr>, String, Type),
    SubIndex(Rc<Expr>, u64, Type),
    SubAccess(Rc<Expr>, Rc<Expr>, Type),
    DoPrim(PrimOp, Vec<Expr>, Vec<u64>, Type),
}

impl Doc for Expr {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            Expr::Reference(name, _) => pretty::Doc::text(name),
            Expr::SubField(expr, name, _) => {
                expr.doc()
                    .append(pretty::Doc::text("."))
                    .append(pretty::Doc::text(name))
            },
            Expr::SubIndex(expr, value, _) => {
                expr.doc()
                    .append(pretty::Doc::text("["))
                    .append(pretty::Doc::as_string(value))
                    .append(pretty::Doc::text("]"))
            },
            Expr::SubAccess(e1, e2, _) => {
                e1.doc()
                    .append(pretty::Doc::text("["))
                    .append(e2.doc())
                    .append(pretty::Doc::text("]"))
            },
            Expr::DoPrim(op, args, consts, _) => {
                let mut doc = op.doc()
                    .append(pretty::Doc::text("("))
                    .append(pretty::Doc::intersperse(
                        args.iter().map(|i| i.doc()),
                        pretty::Doc::text(", ")));
                if consts.len() > 0 {
                    doc = doc.append(pretty::Doc::text(", "))
                            .append(pretty::Doc::intersperse(
                                consts.iter().map(|i| pretty::Doc::as_string(i)),
                                pretty::Doc::text(", ")));
                }
                doc = doc.append(pretty::Doc::text(")"));
                 doc
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Dir {
    Input,
    Output
}

impl Doc for Dir {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            Dir::Input => pretty::Doc::text("input"),
            Dir::Output => pretty::Doc::text("output"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum DefPort {
    Port(Info, String, Dir, Type),
}

impl Doc for DefPort {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            DefPort::Port(info, name, dir, tpe) => {
                dir.doc()
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(name))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(":"))
                    .append(pretty::Doc::space())
                    .append(tpe.doc())
                    .append(info.doc())
                    .append(pretty::Doc::newline()).group()
            }
        }
    }
}

#[derive(Clone, Debug)]
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
    Tail,
    AsUInt,
    AsSInt,
}

impl Doc for PrimOp {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            PrimOp::Add => pretty::Doc::text("add"),
            PrimOp::Sub => pretty::Doc::text("sub"),
            PrimOp::Mul => pretty::Doc::text("mul"),
            PrimOp::Div => pretty::Doc::text("div"),
            PrimOp::Rem => pretty::Doc::text("rem"),
            PrimOp::Lt => pretty::Doc::text("lt"),
            PrimOp::Leq => pretty::Doc::text("leq"),
            PrimOp::Gt => pretty::Doc::text("gt"),
            PrimOp::Geq => pretty::Doc::text("geq"),
            PrimOp::Eq => pretty::Doc::text("eq"),
            PrimOp::Neq => pretty::Doc::text("neq"),
            PrimOp::Pad => pretty::Doc::text("pad"),
            PrimOp::Shl => pretty::Doc::text("shl"),
            PrimOp::Shr => pretty::Doc::text("shr"),
            PrimOp::Dshl => pretty::Doc::text("dshl"),
            PrimOp::Dshr => pretty::Doc::text("dshr"),
            PrimOp::Cvt => pretty::Doc::text("cvt"),
            PrimOp::Neg => pretty::Doc::text("neg"),
            PrimOp::Not => pretty::Doc::text("not"),
            PrimOp::And => pretty::Doc::text("and"),
            PrimOp::Or => pretty::Doc::text("or"),
            PrimOp::Xor => pretty::Doc::text("xor"),
            PrimOp::Andr => pretty::Doc::text("andr"),
            PrimOp::Orr => pretty::Doc::text("orr"),
            PrimOp::Xorr => pretty::Doc::text("xorr"),
            PrimOp::Cat => pretty::Doc::text("cat"),
            PrimOp::Bits => pretty::Doc::text("bits"),
            PrimOp::Head => pretty::Doc::text("head"),
            PrimOp::Tail => pretty::Doc::text("tail"),
            PrimOp::AsUInt => pretty::Doc::text("asUInt"),
            PrimOp::AsSInt => pretty::Doc::text("asSInt"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    EmptyStmt,
    DefInstance(Info, String, String),
    DefNode(Info, String, Expr),
    Block(Vec<Stmt>),
    Connect(Info, Expr, Expr),
}

impl Doc for Stmt {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            Stmt::EmptyStmt => pretty::Doc::text("skip"),
            Stmt::DefInstance(info, name, module) => {
                pretty::Doc::text("inst")
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(name))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text("of"))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(module))
                    .append(info.doc()).group()
            },
            Stmt::DefNode(info, name, expr) => {
                pretty::Doc::text("node")
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(name))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text("="))
                    .append(pretty::Doc::space())
                    .append(expr.doc())
                    .append(info.doc()).group()
            },
            Stmt::Block(stmts) => {
                let mut doc = pretty::Doc::text("");
                for s in stmts {
                    doc = doc.append(s.doc())
                        .append(pretty::Doc::newline());
                }
                doc
            },
            Stmt::Connect(info, loc, expr) => {
                pretty::Doc::text("")
                    .append(loc.doc())
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text("<="))
                    .append(pretty::Doc::space())
                    .append(expr.doc())
                    .append(info.doc()).group()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Param {
    IntParam(String, i64),
    StringParam(String, String),
}

impl Doc for Param {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            Param::IntParam(name, value) => {
                pretty::Doc::text("parameter")
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(name))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text("="))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::as_string(value)).group()
            },
            Param::StringParam(name, value) => {
                pretty::Doc::text("parameter")
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(name))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text("="))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(value)).group()
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum DefModule {
    Module(Info, String, Vec<DefPort>, Stmt),
    ExtModule(Info, String, Vec<DefPort>, String, Vec<Param>)
}

impl Doc for DefModule {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            DefModule::Module(info, name, ports, stmt) => {
                let mut doc = pretty::Doc::text("module")
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(name))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(":"))
                    .append(info.doc())
                    .append(pretty::Doc::newline()).group();
                for p in ports {
                    doc = doc.append(p.doc());
                }
                doc = doc.append(stmt.doc())
                    .nest(2).group();
                doc
            }
            DefModule::ExtModule(info, name, ports, defname, _) => {
                let mut doc = pretty::Doc::text("extmodule")
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(name))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(":"))
                    .append(info.doc())
                    .append(pretty::Doc::newline()).group();
                for p in ports {
                    doc = doc.append(p.doc());
                }
                    doc = doc.append(pretty::Doc::text("defname"))
                        .append(pretty::Doc::space())
                        .append(pretty::Doc::text("="))
                        .append(pretty::Doc::space())
                        .append(pretty::Doc::text(defname))
                        .nest(2).group();
                doc
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum DefCircuit {
    Circuit(Info, Vec<DefModule>, String),
}

impl Doc for DefCircuit {
    fn doc(&self) -> pretty::Doc<pretty::BoxDoc<()>> {
        match self {
            DefCircuit::Circuit(info, modules, main) => {
                let mut doc = pretty::Doc::text("circuit")
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(main))
                    .append(pretty::Doc::space())
                    .append(pretty::Doc::text(":"))
                    .append(info.doc()).group();
                for m in modules {
                    doc = doc.append(pretty::Doc::newline())
                        .append(m.doc())
                        .append(pretty::Doc::newline());
                }
                doc.nest(2).group()
            }
        }
    }
}

#[cfg(test)]
mod tests {
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
        assert_eq!(NoInfo.pretty(), expect);
    }

    #[test]
    fn test_info_fileinfo() {
        let info = "FooBar";
        let expect = format!(" @[{}]", info);
        assert_eq!(FileInfo(info.to_string()).pretty(), expect);
    }

    #[test]
    fn test_unknown_width() {
        let expect = format!("");
        assert_eq!(UnknownWidth.pretty(), expect);
    }

    #[test]
    fn test_int_width() {
        let w = 43;
        let expect = format!("<{}>", w);
        assert_eq!(IntWidth(w).pretty(), expect);
    }

    #[test]
    fn test_type_clock() {
        let expect = "Clock";
        assert_eq!(Clock.pretty(), expect);
    }

    #[test]
    fn test_type_reset() {
        let expect = "Reset";
        assert_eq!(Reset.pretty(), expect);
    }

    #[test]
    fn test_type_unknown() {
        let expect = "?";
        assert_eq!(UnknownType.pretty(), expect);
    }

    #[test]
    fn test_type_uint() {
        let w = 3;
        let expect = format!("UInt<{}>", w);
        assert_eq!(UInt(IntWidth(w)).pretty(), expect);
    }

    #[test]
    fn test_type_sint() {
        let w = 32;
        let expect = format!("SInt<{}>", w);
        assert_eq!(SInt(IntWidth(w)).pretty(), expect);
    }

    #[test]
    fn test_type_fixed_unknown_width() {
        let w = 32;
        let expect = format!("Fixed<{}>", w);
        assert_eq!(Fixed(IntWidth(w), UnknownWidth).pretty(), expect);
    }

    #[test]
    fn test_type_fixed() {
        let w = 32;
        let x = 3;
        let expect = format!("Fixed<{}><{}>", w, x);
        assert_eq!(Fixed(IntWidth(w), IntWidth(x)).pretty(), expect);
    }

    #[test]
    fn test_type_vector() {
        let s = 10;
        let w = 32;
        let t = UInt(IntWidth(w));
        let expect = format!("UInt<{}>[{}]", w, s);
        assert_eq!(Vector(Rc::new(t), s).pretty(), expect);
    }

    #[test]
    fn test_expr_reference() {
        let n = "foo";
        let w = 64;
        let t = UInt(IntWidth(w));
        let expect = format!("{}", n);
        assert_eq!(Reference(n.to_string(), t).pretty(), expect);
    }

    #[test]
    fn test_expr_subfield() {
        let i = "b";
        let f = "n";
        let t = UnknownType;
        let u = UnknownType;
        let expr = Rc::new(Reference(i.to_string(), t));
        let expect = format!("{}.{}", i, f);
        assert_eq!(SubField(expr, f.to_string(), u).pretty(), expect);
    }

    #[test]
    fn test_expr_subindex() {
        let i = "z";
        let a = 10;
        let t = UnknownType;
        let u = UnknownType;
        let expect = format!("{}[{}]", i, a);
        let expr = Rc::new(Reference(i.to_string(), t));
        assert_eq!(SubIndex(expr, a, u).pretty(), expect);
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
        assert_eq!(SubAccess(expr1, expr2, v).pretty(), expect);
    }

    fn test_primops(op: PrimOp, expect: &str) {
        assert_eq!(op.pretty(), expect);
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
    fn test_primops_as_uint() {
        test_primops(AsUInt, "asUInt");
    }

    #[test]
    fn test_primops_as_sint() {
        test_primops(AsSInt, "asSInt");
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
        assert_eq!(DoPrim(Add, expr, vec![], UInt(IntWidth(w))).pretty(), expect);
    }

    #[test]
    fn test_expr_primops_bits() {
        let op1 = "a";
        let w = 32;
        let l = 3;
        let h = 5;
        let expr = vec![Reference(op1.to_string(), UInt(IntWidth(w)))];
        let expect = format!("bits({}, {}, {})", op1, h, l);
        assert_eq!(DoPrim(Bits, expr, vec![h, l], UInt(IntWidth(h - l + 1))).pretty(), expect);
    }

    #[test]
    fn test_dir_input() {
        let expect = format!("{}", "input");
        assert_eq!(Input.pretty(), expect);
    }

    #[test]
    fn test_dir_output() {
        let expect = format!("{}", "output");
        assert_eq!(Output.pretty(), expect);
    }

    #[test]
    fn test_port_input() {
        let i = NoInfo;
        let n = "in";
        let d = Input;
        let w = 32;
        let t = UInt(IntWidth(w));
        let expect = format!("input {} : UInt<{}>\n", n, w);
        assert_eq!(Port(i, n.to_string(), d, t).pretty(), expect);
    }

    #[test]
    fn test_port_output() {
        let i = NoInfo;
        let n = "out";
        let d = Output;
        let w = 32;
        let t = UInt(IntWidth(w));
        let expect = format!("output {} : UInt<{}>\n", n, w);
        assert_eq!(Port(i, n.to_string(), d, t).pretty(), expect);
    }

    #[test]
    fn test_stmt_empty() {
        let expect = format!("{}", "skip");
        assert_eq!(EmptyStmt.pretty(), expect);
    }

    #[test]
    fn test_stmt_node() {
        let n = "n0";
        let r = "a";
        let expr = Reference(r.to_string(), UInt(IntWidth(32)));
        let expect = format!("node {} = {}", n, r);
        assert_eq!(DefNode(NoInfo, n.to_string(), expr).pretty(), expect);
    }

    #[test]
    fn test_stmt_instance() {
        let i = "a0";
        let m = "adder";
        let expect = format!("inst {} of {}", i, m);
        assert_eq!(DefInstance(NoInfo, i.to_string(), m.to_string()).pretty(), expect);
    }

    #[test]
    fn test_stmt_block() {
        let stmts = vec![EmptyStmt, EmptyStmt];
        let expect = format!("{}\n{}\n", "skip", "skip");
        assert_eq!(Block(stmts).pretty(), expect);
    }

    #[test]
    fn test_stmt_connect() {
        let op1 = "a";
        let op2 = "b";
        let expr1 = Reference(op1.to_string(), UnknownType);
        let expr2 = Reference(op2.to_string(), UnknownType);
        let expect = format!("{} <= {}", op1, op2);
        assert_eq!(Connect(NoInfo, expr1, expr2).pretty(), expect);
    }

    #[test]
    fn test_param_int() {
        let name = "WIDTH";
        let val = 3;
        let expect = format!("parameter {} = {}", name, val);
        assert_eq!(IntParam(name.to_string(), val).pretty(), expect);
    }

    #[test]
    fn test_param_string() {
        let name = "ADDR";
        let val = "{32'h00, 32'h01}";
        let expect = format!("parameter {} = {}", name, val);
        assert_eq!(StringParam(name.to_string(), val.to_string()).pretty(), expect);
    }

    #[test]
    fn test_defmodule_extmodule() {
        let n = "foo";
        let d = "bar";
        let expect = format!("extmodule {} :\n  defname = {}", n, d);
        assert_eq!(ExtModule(NoInfo, n.to_string(), vec![], d.to_string(), vec![]).pretty(), expect);
    }

    #[test]
    fn test_defmodule_module() {
        let n = "foo";
        let expect = format!("module {} :\n  skip", n);
        assert_eq!(Module(NoInfo, n.to_string(), vec![], EmptyStmt).pretty(), expect);
    }

    #[test]
    fn test_circuit() {
        let n = "top";
        let expect = format!("circuit {} :", n);
        assert_eq!(Circuit(NoInfo, vec![], n.to_string()).pretty(), expect);
    }
}
