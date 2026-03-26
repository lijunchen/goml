use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    let crate_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let repo_root = crate_dir
        .parent()
        .and_then(Path::parent)
        .unwrap()
        .to_path_buf();

    println!(
        "cargo:rerun-if-changed={}",
        repo_root.join(".git/HEAD").display()
    );

    if let Some(head_ref) = git_stdout(&repo_root, &["symbolic-ref", "--quiet", "HEAD"]) {
        let ref_path = repo_root.join(".git").join(head_ref);
        if ref_path.exists() {
            println!("cargo:rerun-if-changed={}", ref_path.display());
        }
    }

    if let Some(hash) = git_stdout(&repo_root, &["rev-parse", "--short=9", "HEAD"]) {
        println!("cargo:rustc-env=GOML_GIT_HASH={hash}");
    }

    if let Some(date) = git_stdout(
        &repo_root,
        &[
            "show",
            "-s",
            "--date=format:%Y-%m-%d",
            "--format=%cd",
            "HEAD",
        ],
    ) {
        println!("cargo:rustc-env=GOML_GIT_DATE={date}");
    }
}

fn git_stdout(repo_root: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(repo_root)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let stdout = String::from_utf8(output.stdout).ok()?;
    let trimmed = stdout.trim();
    if trimmed.is_empty() {
        return None;
    }
    Some(trimmed.to_string())
}
