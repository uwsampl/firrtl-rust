use firrtl::*;

use Info::*;
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
    let port = Port(NoInfo, "in".into(), Input, UInt(32));
    let module = Module(NoInfo, name.into(), vec![port], EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], name.into());
    let expect = format!("module {}(\n  input  [31:0] in\n);\n  initial begin end\nendmodule\n", &name);
    emit(cir, &name);
    assert_eq!(read_verilog(&name), expect);
}