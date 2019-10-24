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
    let cir = Circuit(NoInfo, vec![], "top".into()).to_pretty();
    println!("{}", cir);
}