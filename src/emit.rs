use std::fs::File;
use std::io::{self, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::ast::DefCircuit;
use crate::ast::ToDoc;

fn path_to_string(path: &Path) -> String {
    match path.to_str() {
        Some(s) => String::from(s),
        None => panic!("failed to convert path to string"),
    }
}

fn compiler(firrtl: &Path, verilog: &Path) {
    let mut cmd_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    cmd_path.push("firrtl/utils/bin/firrtl");
    let mut args = Vec::new();
    let fpath = path_to_string(firrtl);
    let vpath = path_to_string(verilog);
    args.push("-i");
    args.push(&fpath);
    args.push("-o");
    args.push(&vpath);
    args.push("-X");
    args.push("verilog");
    let cmd = Command::new(&cmd_path)
        .args(&args)
        .output()
        .expect("failed to compile firrtl");
    io::stdout().write_all(&cmd.stdout).unwrap();
    io::stderr().write_all(&cmd.stderr).unwrap();
    assert!(cmd.status.success(), "failed to compile firrtl");
}

pub fn firrtl(cir: &DefCircuit, path: &Path) -> std::io::Result<()> {
    let mut buffer = BufWriter::new(File::create(path)?);
    buffer.write_all(cir.to_pretty().as_bytes())?;
    buffer.flush()?;
    Ok(())
}

pub fn verilog(cir: &DefCircuit, path: &Path) {
    let name = match path.file_stem() {
        Some(n) => match n.to_str() {
            Some(s) => s,
            _ => panic!("Filename extraction failed"),
        },
        None => panic!("Filename extraction failed"),
    };
    let mut fpath = path.with_file_name(&name).to_path_buf();
    fpath.set_extension("fir");
    firrtl(cir, &fpath).unwrap();
    compiler(&fpath, path);
}

#[cfg(test)]
mod tests {
    use crate::ast::DefCircuit::*;
    use crate::ast::DefModule::*;
    use crate::ast::DefPort::*;
    use crate::ast::Dir::*;
    use crate::ast::Expr::*;
    use crate::ast::Info::*;
    use crate::ast::Stmt::*;
    use crate::ast::Type::*;
    use crate::ast::Width::*;
    use crate::emit;
    use crate::read_verilog;
    use std::path::PathBuf;
    use std::rc::Rc;

    fn test_path(name: &str) -> PathBuf {
        let mut path = PathBuf::new();
        path.push(format!("{}.v", name));
        path
    }

    #[test]
    fn module_empty_stmt() {
        let name = "module_empty_stmt";
        let module = Module(NoInfo, name.to_string(), vec![], EmptyStmt);
        let cir = Circuit(NoInfo, vec![module], name.to_string());
        let expect = format!("module {}(\n);\n  initial begin end\nendmodule\n", &name);
        emit::verilog(&cir, &test_path(name));
        assert_eq!(read_verilog(&test_path(name)), expect);
    }

    #[test]
    fn module_one_port() {
        let name = "module_one_port";
        let port = Port(NoInfo, "in".to_string(), Input, UInt(IntWidth(32)));
        let module = Module(NoInfo, name.to_string(), vec![port], EmptyStmt);
        let cir = Circuit(NoInfo, vec![module], name.to_string());
        let expect = format!(
            "module {}(\n  input  [31:0] in\n);\n  initial begin end\nendmodule\n",
            &name
        );
        emit::verilog(&cir, &test_path(name));
        assert_eq!(read_verilog(&test_path(name)), expect);
    }

    #[test]
    fn extmodule_one_port() {
        let name = "extmodule_one_port";
        let mod_name = "mod";
        let ext_name = "ext";
        let ins_name = "i";
        let ver_name = "verilog_module";
        let port_name = "in";
        let w = 32;
        let mut stmts = Vec::new();
        let mut modules = Vec::new();
        let port = vec![Port(
            NoInfo,
            port_name.to_string(),
            Input,
            UInt(IntWidth(w)),
        )];
        let eport = vec![Port(
            NoInfo,
            port_name.to_string(),
            Input,
            UInt(IntWidth(w)),
        )];
        let expr1 = SubField(
            Rc::new(Reference(ins_name.to_string(), UnknownType)),
            port_name.to_string(),
            UInt(IntWidth(w)),
        );
        let expr2 = Reference(port_name.to_string(), UInt(IntWidth(w)));
        stmts.push(DefInstance(
            NoInfo,
            ins_name.to_string(),
            ext_name.to_string(),
        ));
        stmts.push(Connect(NoInfo, expr1, expr2));
        modules.push(ExtModule(
            NoInfo,
            ext_name.to_string(),
            eport,
            ver_name.to_string(),
            vec![],
        ));
        modules.push(Module(NoInfo, mod_name.to_string(), port, Block(stmts)));
        let cir = Circuit(NoInfo, modules, mod_name.to_string());
        let mut expect = String::new();
        expect.push_str(&format!("module {}(\n  input  [31:0] in\n);", mod_name));
        expect.push_str(&format!("\n  wire [31:0] {}_{};", ins_name, port_name));
        expect.push_str(&format!(
            "\n  {} {} (\n    .{}({}_{})\n  );",
            ver_name, ins_name, port_name, ins_name, port_name
        ));
        expect.push_str(&format!(
            "\n  assign {}_{} = {};\nendmodule\n",
            ins_name, port_name, port_name
        ));
        emit::verilog(&cir, &test_path(name));
        assert_eq!(read_verilog(&test_path(name)), expect);
    }
}
