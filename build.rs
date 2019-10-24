use std::process::Command;
use std::path::Path;

fn main () {
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
}