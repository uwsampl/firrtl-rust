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
    assert_eq!(NoInfo.to_pretty(), "");
}