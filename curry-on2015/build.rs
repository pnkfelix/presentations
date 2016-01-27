extern crate tango;
extern crate pandoc;

use pandoc::{Pandoc, PandocError};

use std::convert;
use std::env;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

fn main() {
    tango::process_root().unwrap();

    run_pandoc(".", "../curry-on2015").unwrap();
}

fn is_skipped_md(entry: &fs::DirEntry) -> bool {
    {
        match entry.path().file_name().and_then(|p|p.to_str()) {
            Some("mod.md") | Some("lib.md") => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    IoError(io::Error),
    IoErrorContext(io::Error, ErrorContext),
    OutputError(std::process::Output),
}

impl convert::From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
    }
}

impl convert::From<PandocError> for Error {
    fn from(e: PandocError) -> Self {
        match e {
            PandocError::IoErr(e) => Error::IoError(e),
            PandocError::Err(out) => Error::OutputError(out),
        }
    }
}

#[derive(Debug)]
pub enum ErrorContext {
    ReadDir(PathBuf),
}

fn read_dir<P: AsRef<Path>>(path: P) -> Result<fs::ReadDir, Error> {
    let path = path.as_ref();
    fs::read_dir(path).map_err(|e| {
        Error::IoErrorContext(e, ErrorContext::ReadDir(path.to_path_buf()))
    })
}

fn run_pandoc(src_dir: &str, target_name: &str) -> Result<(), Error> {
    let mut pandoc = pandoc::new();

    let src_dir_path = &format!("src/{}", src_dir);
    for entry in try!(read_dir(src_dir_path)) {
        let entry = try!(entry);
        if is_skipped_md(&entry) { continue; }
        if let Some("md") = entry.path().extension().and_then(|p|p.to_str()) {
            pandoc.add_input(entry.path().to_path_buf().to_str().unwrap());
        }
    }

    let tgt_path = format!("{}.html", target_name);
    pandoc.set_output(tgt_path);

    pandoc.set_output_format(pandoc::OutputFormat::Revealjs);
    pandoc.set_variable("theme", "mozilla-sandstone");
    pandoc.set_variable("center", "false");
    pandoc.set_slide_level(2);

    run_cmd(pandoc)
}

fn run_cmd(pandoc: pandoc::Pandoc) -> Result<(), Error> {
    try!(pandoc.execute());
    Ok(())
}

#[cfg(not_now)]
fn run_cmd(pandoc: pandoc::Pandoc) -> Result<(), Error> {
    let cmd = pandoc.cmd();

    cmd.arg("-s");
    cmd.args(&["--css", "slide-style.css"]);
    cmd.args(&["--css", "code-style.css"]);

    let command = format!("{:?}", pandoc);
    println!("command: {}", command);
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

#[cfg(cmd_pandoc)]
fn main() {
    use pandoc::PandocOption as O;
    use pandoc::OutputFormat as F;

    tango::process_root().unwrap();

    // We must run pandoc's processing after tango since tango may
    // generate `.md` files that we want to process.
    pandoc::run_pandoc(".", "../curry-on2015",
                       &[O::Standalone,
                         O::To(F::revealjs),
                         O::Var("theme", Some("mozilla-sandstone")),
                         O::Var("center", Some("false")),
                         O::Css("slide-style.css"),
                         O::Css("code-style.css"),
                         // O::Css("fonts.css"),

                         // Setting this explicitly will hopefully avoid the whole
                         // document going beserk if I happen to follow an H1 by content
                         // with no intervening H2.
                         O::SlideLevel(2),

                         ]).unwrap();
}
