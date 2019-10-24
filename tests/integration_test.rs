use firrtl::*;

use Info::*;
use Type::*;
use Expr::*;
use PrimOp::*;
use Dir::*;
use DefPort::*;
use DefModule::*;
use DefCircuit::*;

#[test]
fn no_info() {
    let cir = Circuit(NoInfo, vec![], "top".into());
    // emitFirrtl(cir, "foo.fir");
    assert_eq!("", "");
}