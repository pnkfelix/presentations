extern crate tango;
extern crate cmd_pandoc as pandoc;

fn main() {
    use pandoc::PandocOption as O;
    use pandoc::OutputFormat as F;
    use pandoc::OutputFormatExt as E;

    tango::process_root().unwrap();

    // We must run pandoc's processing after tango since tango may
    // generate `.md` files that we want to process.
    pandoc::run_pandoc("slides", "../cd-kickoff-2016",
                       &[O::Standalone,
                         O::To(E::FmtExt(F::revealjs, vec![format!("fenced_code_attributes")])),
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

    fn exercise_page(source_dir: &str, target_file: &str) -> pandoc::Result<()>{
        pandoc::run_pandoc(source_dir,
                           target_file,
                           &[O::Standalone,
                             O::To(E::Fmt(F::html5)),
                             O::Css("exercise-style.css"),
                             ])
    }

    exercise_page("exercises/", "../cd-kickoff-exercises").unwrap();

    for ex in &["hello", "ownership", "borrowing",
                "structs", "threads", "closures", "enums_match",
                "lifetimes", "iterators"] {
        exercise_page(&format!("exercises/{}", ex),
                      &format!("../cd-kickoff-{}", ex)).unwrap();
    }
}
