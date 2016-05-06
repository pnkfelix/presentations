extern crate tango;
extern crate cmd_pandoc as pandoc;

fn main() {
    use pandoc::PandocOption as O;
    use pandoc::OutputFormat as F;
    use pandoc::OutputFormatExt as FE;

    tango::process_root().unwrap();

    // We must run pandoc's processing after tango since tango may
    // generate `.md` files that we want to process.
    pandoc::run_pandoc(".", "../fiability2016",
                       &[O::Standalone,
                         O::To(FE::Fmt(F::revealjs)),
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
