use pretty::{Doc, BoxDoc};

pub type Info = String;

pub enum Type {
    UInt{width: u64},
}

use Type::*;

impl Type {
    pub fn to_doc(&self) -> Doc<BoxDoc<()>> {
        match *self {
            UInt{width} => {
                Doc::concat(
                    vec![
                        Doc::text("UInt"),
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
    fn test_info() {
        assert_eq!(Info::from("hello"), "hello");
    }

    #[test]
    fn test_uint() {
        assert_eq!(UInt{width:3}.to_pretty(), "UInt<3>");
    }
}