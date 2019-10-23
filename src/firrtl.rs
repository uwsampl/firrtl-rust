use pretty::{Doc, BoxDoc};

pub enum FirrtlNode {
    NoInfo,
}

use FirrtlNode::*;

impl FirrtlNode {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            NoInfo => Doc::text(""),
        }
    }
    // given that firrtl has a spec format, we should
    // set the width accordingly? using 100 for now
    pub fn to_pretty(&self) -> String {
        let width: usize = 100;
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

#[cfg(test)]
mod test{
    use super::*;

    #[test]
    fn test_no_info() {
        assert_eq!(NoInfo.to_pretty(), "");
    }
}