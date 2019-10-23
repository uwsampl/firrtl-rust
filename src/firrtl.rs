use pretty::{Doc, BoxDoc};

pub enum Info {
    NoInfo,
}

pub enum Width {
    IntWidth {
        width: i32,
    }
}

pub enum Firrtl {
    NodeInfo(Info),
    NodeWidth(Width),
}

use Firrtl::*;

impl Firrtl {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            NodeInfo(Info::NoInfo) => Doc::text(""),
            NodeWidth(Width::IntWidth{width}) => {
                Doc::concat(
                    vec![
                        Doc::text("<"),
                        Doc::as_string(width),
                        Doc::text(">")
                    ]
                )
            },
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
        assert_eq!(NodeInfo(Info::NoInfo).to_pretty(), "");
    }

    #[test]
    fn test_int_width() {
        assert_eq!(NodeWidth(Width::IntWidth{width:3}).to_pretty(), "<3>");
    }
}