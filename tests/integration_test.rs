#[test]
fn test_integration() {
    use firrtl::emit;
    use firrtl::read_verilog;
    use firrtl::ast::Info::*;
    use firrtl::ast::Stmt::*;
    use firrtl::ast::DefModule::*;
    use firrtl::ast::DefCircuit::*;
    let name = "test_integration";
    let path = format!("{}.v", name);
    let module = Module(NoInfo, name.to_string(), vec![], EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], name.to_string());
    let expect = format!("module {}(\n);\n  initial begin end\nendmodule\n", &name);
    emit::verilog(&cir, &path);
    assert_eq!(read_verilog(&path), expect);
}

