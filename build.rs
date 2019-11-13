use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    let firrtl = "firrtl";
    let work_dir = Path::new(".").join(firrtl);
    if !work_dir.exists() {
        let mut git_args = Vec::new();
        git_args.push("clone");
        git_args.push("https://github.com/freechipsproject/firrtl.git");
        git_args.push(work_dir.to_str().unwrap());
        let git = Command::new("git")
            .args(&git_args)
            .output()
            .expect("failed to git clone firrtl");
        assert!(git.status.success(), "failed to clone firrtl");
    }

    let firrtl_jar = work_dir.join("utils/bin/firrtl.jar");
    if !firrtl_jar.exists() {
        // cd firrtl
        assert!(env::set_current_dir(&work_dir).is_ok());
        // sbt compile
        let sbt_compile = Command::new("sbt")
            .args(&["compile"])
            .output()
            .expect("failed to sbt compile firrtl");
        assert!(sbt_compile.status.success(), "failed to sbt compile firrtl");
        // sbt assembly
        let sbt_assembly = Command::new("sbt")
            .args(&["assembly"])
            .output()
            .expect("failed to sbt assembly firrtl");
        assert!(
            sbt_assembly.status.success(),
            "failed to sbt assembly firrtl"
        );
    }
}
