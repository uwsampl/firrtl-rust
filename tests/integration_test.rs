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
    let name = "top";
    let module = Module(NoInfo, name.into(), vec![], EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], name.into());
    let expect = "module top(\n);\n  initial begin end\nendmodule\n";
    emit(cir, &name);
    assert_eq!(read_verilog(&name), expect);
}