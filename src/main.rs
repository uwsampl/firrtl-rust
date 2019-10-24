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
    let name = "top";
    let port = vec![Port(NoInfo, "i0".into(), Input, UInt(32)), Port(NoInfo, "i1".into(), Input, UInt(32))];
    let module = Module(NoInfo, name.into(), port, EmptyStmt);
    let cir = Circuit(NoInfo, vec![module], name.into());
    emit(cir, "two");
}