use std::ffi::OsStr;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use anyhow::{Context, anyhow, bail};
use goml_project::config::goml_std_dir;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

const FORMAT_VERSION: &[u8] = b"goml-build-cache-v1";

pub(crate) struct CacheInput<'a> {
    role: &'static str,
    path: &'a Path,
}

impl<'a> CacheInput<'a> {
    pub(crate) fn new(role: &'static str, path: &'a Path) -> Self {
        Self { role, path }
    }
}

pub(crate) struct CompilerIdentity {
    digest: [u8; 32],
    stamp: [u8; 32],
}

impl CompilerIdentity {
    pub(crate) fn read(executable: &Path, working_dir: &Path) -> anyhow::Result<Self> {
        let canonical = executable.canonicalize().with_context(|| {
            format!(
                "failed to resolve compiler identity for {}",
                executable.display()
            )
        })?;
        let mut hasher = Sha256::new();
        update_bytes(&mut hasher, FORMAT_VERSION);
        update_os_str(&mut hasher, canonical.as_os_str());
        update_file(&mut hasher, &canonical)?;
        if let Some(stdlib) = stdlib_root(working_dir) {
            update_bytes(&mut hasher, b"stdlib");
            update_os_str(&mut hasher, stdlib.as_os_str());
            if stdlib.join("goml.toml").is_file() {
                update_stdlib(&mut hasher, &stdlib)?;
            } else {
                update_bytes(&mut hasher, b"missing");
            }
        } else {
            update_bytes(&mut hasher, b"no-stdlib");
        }
        Ok(Self {
            digest: hasher.finalize().into(),
            stamp: toolchain_stamp(executable, working_dir)?,
        })
    }

    pub(crate) fn ensure_unchanged(
        &mut self,
        executable: &Path,
        working_dir: &Path,
    ) -> anyhow::Result<()> {
        let stamp = toolchain_stamp(executable, working_dir)?;
        if stamp == self.stamp {
            return Ok(());
        }
        let current = Self::read(executable, working_dir)?;
        if current.digest != self.digest {
            bail!(
                "compiler or standard library changed while executing the command plan: {}",
                executable.display()
            );
        }
        self.stamp = current.stamp;
        Ok(())
    }
}

pub(crate) struct CommandCache<'a> {
    module_dir: &'a Path,
    compiler: [u8; 32],
    kind: &'static str,
    args: &'a [std::ffi::OsString],
    inputs: Vec<CacheInput<'a>>,
    outputs: Vec<PathBuf>,
    path: PathBuf,
}

#[derive(Deserialize, Serialize)]
struct CacheRecord {
    fingerprint: String,
    outputs: Vec<String>,
}

impl<'a> CommandCache<'a> {
    pub(crate) fn new(
        module_dir: &'a Path,
        compiler: &CompilerIdentity,
        kind: &'static str,
        args: &'a [std::ffi::OsString],
        inputs: Vec<CacheInput<'a>>,
        outputs: Vec<PathBuf>,
        primary_output: &'a Path,
    ) -> Self {
        let mut path = absolute_from_module(module_dir, primary_output).into_os_string();
        path.push(format!(".goml-{kind}-fingerprint"));
        Self {
            module_dir,
            compiler: compiler.digest,
            kind,
            args,
            inputs,
            outputs,
            path: PathBuf::from(path),
        }
    }

    pub(crate) fn is_fresh(&self, fingerprint: &str) -> anyhow::Result<bool> {
        let data = match fs::read(&self.path) {
            Ok(data) => data,
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(false),
            Err(error) => {
                return Err(error)
                    .with_context(|| format!("failed to read {}", self.path.display()));
            }
        };
        let Ok(record) = serde_json::from_slice::<CacheRecord>(&data) else {
            return Ok(false);
        };
        if record.fingerprint != fingerprint {
            return Ok(false);
        }
        let Some(outputs) = self.output_digests()? else {
            return Ok(false);
        };
        Ok(record.outputs == outputs)
    }

    pub(crate) fn prepare_for_execution(&self) -> anyhow::Result<()> {
        remove_if_present(&self.path)?;
        for output in &self.outputs {
            remove_if_present(&absolute_from_module(self.module_dir, output))?;
        }
        Ok(())
    }

    pub(crate) fn store_if_unchanged(&self, before: &str) -> anyhow::Result<()> {
        let after = match self.fingerprint() {
            Ok(after) => after,
            Err(error) => {
                self.prepare_for_execution()?;
                return Err(error).context("cache input changed while executing subcommand");
            }
        };
        if after != before {
            self.prepare_for_execution()?;
            bail!("cache input changed while executing subcommand");
        }
        let outputs = match self.output_digests() {
            Ok(Some(outputs)) => outputs,
            Ok(None) => {
                let message = anyhow!(
                    "subcommand succeeded without all expected outputs: {}",
                    self.outputs
                        .iter()
                        .map(|path| absolute_from_module(self.module_dir, path)
                            .display()
                            .to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                self.prepare_for_execution()?;
                return Err(message);
            }
            Err(error) => {
                self.prepare_for_execution()?;
                return Err(error);
            }
        };
        let record = CacheRecord {
            fingerprint: after,
            outputs,
        };
        let data = serde_json::to_vec(&record)?;
        let parent = self.path.parent().ok_or_else(|| {
            anyhow!(
                "fingerprint path {} has no parent directory",
                self.path.display()
            )
        })?;
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
        let mut temporary = self.path.as_os_str().to_os_string();
        temporary.push(format!(".tmp-{}", std::process::id()));
        let temporary = PathBuf::from(temporary);
        fs::write(&temporary, data)
            .with_context(|| format!("failed to write {}", temporary.display()))?;
        if let Err(error) = fs::rename(&temporary, &self.path) {
            let _ = fs::remove_file(&temporary);
            return Err(error).with_context(|| {
                format!(
                    "failed to move {} to {}",
                    temporary.display(),
                    self.path.display()
                )
            });
        }
        Ok(())
    }

    pub(crate) fn fingerprint(&self) -> anyhow::Result<String> {
        let mut hasher = Sha256::new();
        update_bytes(&mut hasher, FORMAT_VERSION);
        update_bytes(&mut hasher, &self.compiler);
        update_bytes(&mut hasher, self.kind.as_bytes());
        update_os_str(&mut hasher, self.module_dir.as_os_str());
        update_u64(&mut hasher, self.args.len() as u64);
        for arg in self.args {
            update_os_str(&mut hasher, arg);
        }
        update_u64(&mut hasher, self.inputs.len() as u64);
        for input in &self.inputs {
            update_bytes(&mut hasher, input.role.as_bytes());
            update_os_str(&mut hasher, input.path.as_os_str());
            update_file(
                &mut hasher,
                &absolute_from_module(self.module_dir, input.path),
            )?;
        }
        Ok(format!("{:x}", hasher.finalize()))
    }

    fn output_digests(&self) -> anyhow::Result<Option<Vec<String>>> {
        let mut digests = Vec::with_capacity(self.outputs.len());
        for output in &self.outputs {
            let output = absolute_from_module(self.module_dir, output);
            if !output.is_file() {
                return Ok(None);
            }
            let mut hasher = Sha256::new();
            update_file(&mut hasher, &output)?;
            digests.push(format!("{:x}", hasher.finalize()));
        }
        Ok(Some(digests))
    }
}

fn remove_if_present(path: &Path) -> anyhow::Result<()> {
    match fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(error) if error.kind() == io::ErrorKind::NotFound => Ok(()),
        Err(error) => Err(error).with_context(|| format!("failed to remove {}", path.display())),
    }
}

fn absolute_from_module(module_dir: &Path, path: &Path) -> PathBuf {
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        module_dir.join(path)
    }
}

fn update_file(hasher: &mut Sha256, path: &Path) -> anyhow::Result<()> {
    let file = fs::File::open(path)
        .with_context(|| format!("failed to read cache input {}", path.display()))?;
    let length = file
        .metadata()
        .with_context(|| format!("failed to inspect cache input {}", path.display()))?
        .len();
    update_u64(hasher, length);
    let mut reader = io::BufReader::new(file);
    let mut buffer = [0; 64 * 1024];
    loop {
        let read = reader
            .read(&mut buffer)
            .with_context(|| format!("failed to read cache input {}", path.display()))?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
    }
    Ok(())
}

fn stdlib_root(working_dir: &Path) -> Option<PathBuf> {
    if let Some(path) = std::env::var_os("GOML_STD_PATH")
        && !path.is_empty()
    {
        return Some(absolute_from_module(working_dir, Path::new(&path)));
    }
    let source_tree = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/std");
    if source_tree.join("goml.toml").is_file() {
        return Some(source_tree);
    }
    goml_std_dir()
        .ok()
        .map(|path| absolute_from_module(working_dir, &path))
}

fn update_stdlib(hasher: &mut Sha256, root: &Path) -> anyhow::Result<()> {
    let files = stdlib_files(root)?;
    update_u64(hasher, files.len() as u64);
    for file in files {
        let relative = file.strip_prefix(root).unwrap_or(&file);
        update_os_str(hasher, relative.as_os_str());
        update_file(hasher, &file)?;
    }
    Ok(())
}

fn toolchain_stamp(executable: &Path, working_dir: &Path) -> anyhow::Result<[u8; 32]> {
    let canonical = executable.canonicalize().with_context(|| {
        format!(
            "failed to resolve compiler identity for {}",
            executable.display()
        )
    })?;
    let mut hasher = Sha256::new();
    update_os_str(&mut hasher, canonical.as_os_str());
    update_metadata(&mut hasher, &canonical)?;
    if let Some(stdlib) = stdlib_root(working_dir) {
        update_os_str(&mut hasher, stdlib.as_os_str());
        if stdlib.join("goml.toml").is_file() {
            let files = stdlib_files(&stdlib)?;
            update_u64(&mut hasher, files.len() as u64);
            for file in files {
                update_os_str(
                    &mut hasher,
                    file.strip_prefix(&stdlib).unwrap_or(&file).as_os_str(),
                );
                update_metadata(&mut hasher, &file)?;
            }
        } else {
            update_bytes(&mut hasher, b"missing");
        }
    } else {
        update_bytes(&mut hasher, b"no-stdlib");
    }
    Ok(hasher.finalize().into())
}

fn stdlib_files(root: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut directories = vec![root.to_path_buf()];
    let mut files = vec![root.join("goml.toml")];
    while let Some(directory) = directories.pop() {
        for entry in fs::read_dir(&directory)
            .with_context(|| format!("failed to read cache input {}", directory.display()))?
        {
            let entry = entry
                .with_context(|| format!("failed to read cache input {}", directory.display()))?;
            let path = entry.path();
            if path.is_dir() {
                directories.push(path);
            } else if path.is_file() && path.extension().is_some_and(|extension| extension == "gom")
            {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

fn update_metadata(hasher: &mut Sha256, path: &Path) -> anyhow::Result<()> {
    let metadata = fs::metadata(path)
        .with_context(|| format!("failed to inspect cache input {}", path.display()))?;
    update_u64(hasher, metadata.len());
    if let Ok(modified) = metadata.modified()
        && let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH)
    {
        update_u64(hasher, duration.as_secs());
        update_u64(hasher, duration.subsec_nanos().into());
    }
    update_platform_metadata(hasher, &metadata);
    Ok(())
}

#[cfg(unix)]
fn update_platform_metadata(hasher: &mut Sha256, metadata: &fs::Metadata) {
    use std::os::unix::fs::MetadataExt;

    update_u64(hasher, metadata.dev());
    update_u64(hasher, metadata.ino());
    update_u64(hasher, metadata.ctime() as u64);
    update_u64(hasher, metadata.ctime_nsec() as u64);
}

#[cfg(not(unix))]
fn update_platform_metadata(_hasher: &mut Sha256, _metadata: &fs::Metadata) {}

fn update_bytes(hasher: &mut Sha256, value: &[u8]) {
    update_u64(hasher, value.len() as u64);
    hasher.update(value);
}

fn update_u64(hasher: &mut Sha256, value: u64) {
    hasher.update(value.to_le_bytes());
}

#[cfg(unix)]
fn update_os_str(hasher: &mut Sha256, value: &OsStr) {
    use std::os::unix::ffi::OsStrExt;

    update_bytes(hasher, value.as_bytes());
}

#[cfg(windows)]
fn update_os_str(hasher: &mut Sha256, value: &OsStr) {
    use std::os::windows::ffi::OsStrExt;

    let wide = value.encode_wide().collect::<Vec<_>>();
    update_u64(hasher, wide.len() as u64);
    for code_unit in wide {
        hasher.update(code_unit.to_le_bytes());
    }
}

#[cfg(not(any(unix, windows)))]
fn update_os_str(hasher: &mut Sha256, value: &OsStr) {
    update_bytes(hasher, value.to_string_lossy().as_bytes());
}
