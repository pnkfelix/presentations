use std::convert;
use std::env;
//use std::error::Error as ErrorTrait;
use std::fs;
use std::io;
use std::process::Command;
use std::path::{Path, PathBuf};
use std::result;

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    IoErrorContext(io::Error, ErrorContext),
}

#[derive(Debug)]
pub enum ErrorContext {
    ReadDir(PathBuf),
}

impl convert::From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
    }
}

pub type Result<X> = result::Result<X, Error>;

fn read_dir<P: AsRef<Path>>(path: P) -> Result<fs::ReadDir> {
    let path = path.as_ref();
    fs::read_dir(path).map_err(|e| {
        Error::IoErrorContext(e, ErrorContext::ReadDir(path.to_path_buf()))
    })
}

pub fn run_pandoc(src_name: &str, target_name: &str) -> Result<()> {
    let src_dir_path = &format!("src/{}", src_name);
    let mut src_paths = Vec::new();
    for entry in try!(read_dir(src_dir_path)) {
        let entry = try!(entry);
        if is_mod_md(&entry) { continue; }
        if let Some("md") = entry.path().extension().and_then(|p|p.to_str()) {
            src_paths.push(entry.path());
        }
    }
    let tgt_path = &format!("{}.html", target_name);
    let mut pandoc = Command::new("pandoc");
    pandoc
        .args(&["-o", tgt_path])
        .args(&["-s"]);
    src_paths.sort();
    for p in src_paths {
        pandoc.arg(p);
    }

    let command = format!("{:?}", pandoc);
    match pandoc.output() {
        Ok(ref output) if output.status.success() => {}
        Ok(ref output) => {
            panic!("something went wrong running pandoc; \
                    command: {} current_dir: {} exit status: {:?} stdout: {} stderr: {}",
                   command,
                   env::current_dir().unwrap().display(),
                   output.status.code(),
                   String::from_utf8_lossy(&output.stdout),
                   String::from_utf8_lossy(&output.stderr),
                   );
        }
        Err(e) => {
            panic!("something went wrong running pandoc; \
                    command: {} current_dir: {} err: {} PATH: {:?}",
                   command,
                   env::current_dir().unwrap().display(),
                   e,
                   env::var_os("PATH"));
        }
    }

    Ok(())
}

fn is_mod_md(entry: &fs::DirEntry) -> bool {
    {
        if let Some("mod.md") = entry.path().file_name().and_then(|p|p.to_str()) {
            true
        } else {
            false
        }
    }
}


#[test]
fn it_works() {
}
