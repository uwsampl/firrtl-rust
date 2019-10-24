mod lib;

fn main() {
    use lib::*;
    use Info::*;
    use Type::*;
    use Expr::*;
    use PrimOp::*;
    use Dir::*;
    use DefPort::*;
    use Stmt::*;
    use DefModule::*;
    use DefCircuit::*;
    let module = Module(NoInfo, "top".into(), vec![], EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], "top".into());
    println!("{}", cir.to_pretty());
    emit(cir, "foo");
}