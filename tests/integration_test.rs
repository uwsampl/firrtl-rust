use std::rc::Rc;
use firrtl::*;
use Info::*;
use Width::*;
use Type::*;
use Expr::*;
use PrimOp::*;
use Dir::*;
use Stmt::*;
use DefPort::*;
use DefModule::*;
use DefCircuit::*;

#[test]
fn module_empty_stmt() {
    let name = "module_empty_stmt";
    let module = Module(NoInfo, name.into(), vec![], EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], name.into());
    let expect = format!("module {}(\n);\n  initial begin end\nendmodule\n", &name);
    emit(cir, &name);
    assert_eq!(read_verilog(&name), expect);
}

#[test]
fn module_one_port() {
    let name = "module_one_port";
    let port = Port(NoInfo, "in".into(), Input, UInt(IntWidth(32)));
    let module = Module(NoInfo, name.into(), vec![port], EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], name.into());
    let expect = format!("module {}(\n  input  [31:0] in\n);\n  initial begin end\nendmodule\n", &name);
    emit(cir, &name);
    assert_eq!(read_verilog(&name), expect);
}

#[test]
fn extmodule_one_port() {
    let name = "extmodule_one_port";
    let mod_name = "mod";
    let ext_name = "ext";
    let ins_name = "i";
    let ver_name = "verilog_module";
    let port_name = "in";
    let w = 32;
    let mut stmts = Vec::new();
    let mut modules = Vec::new();
    let port = vec![Port(NoInfo, port_name.into(), Input, UInt(IntWidth(w)))];
    let eport = vec![Port(NoInfo, port_name.into(), Input, UInt(IntWidth(w)))];
    let expr1 = SubField(Rc::new(Reference(ins_name.into(), UnknownType)), port_name.into(), UInt(IntWidth(w)));
    let expr2 = Reference(port_name.into(), UInt(IntWidth(w)));
    stmts.push(DefInstance(NoInfo, ins_name.into(), ext_name.into()));
    stmts.push(Connect(NoInfo, expr1, expr2));
    modules.push(ExtModule(NoInfo, ext_name.into(), eport, ver_name.into(), vec![]));
    modules.push(Module(NoInfo, mod_name.into(), port, Block(stmts)));
    let cir = Circuit(NoInfo, modules, mod_name.into());
    let mut expect = String::new();
    expect.push_str(&format!("module {}(\n  input  [31:0] in\n);", mod_name));
    expect.push_str(&format!("\n  wire [31:0] {}_{};", ins_name, port_name));
    expect.push_str(&format!("\n  {} {} (\n    .{}({}_{})\n  );", ver_name, ins_name, port_name, ins_name, port_name));
    expect.push_str(&format!("\n  assign {}_{} = {};\nendmodule\n", ins_name, port_name, port_name));
    emit(cir, &name);
    assert_eq!(read_verilog(&name), expect);
}