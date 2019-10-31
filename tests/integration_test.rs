use firrtl::*;

#[test]
fn test_integration() {
    use ast::Info::*;
    use ast::Stmt::*;
    use ast::DefModule::*;
    use ast::DefCircuit::*;
    let name = "test_integration";
    let path = format!("{}.v", name);
    let module = Module(NoInfo, name.to_string(), vec![], EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], name.to_string());
    let expect = format!("module {}(\n);\n  initial begin end\nendmodule\n", &name);
    emit::verilog(&cir, &path);
    assert_eq!(read_verilog(&path), expect);
}

