pub mod ast;
pub mod emit;

pub fn read_verilog(path: &str) -> String {
    use std::fs::read_to_string;
    let s = read_to_string(path)
        .expect("Something went wrong reading the file");
    s
}
