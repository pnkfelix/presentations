extern crate tango;
extern crate cmd_pandoc as pandoc;

fn main() {
    tango::process_root().unwrap();

    // We must run pandoc's processing after tango since tango may
    // generate `.md` files that we want to process.
    pandoc::run_pandoc(".", "index").unwrap();
}
