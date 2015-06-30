extern crate tango;
extern crate cmd_pandoc as pandoc;

fn main() {
    use pandoc::PandocOption as O;
    use pandoc::OutputFormat as F;

    tango::process_root().unwrap();

    // We must run pandoc's processing after tango since tango may
    // generate `.md` files that we want to process.
    pandoc::run_pandoc(".", "../curry-on2015",
                       &[O::Standalone,
                         O::To(F::revealjs),
                         // O::Var("theme=clean", None),

                         ]).unwrap();
}
