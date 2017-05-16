extern crate mon_artist;
extern crate pandoc;
extern crate pandoc_ast;
extern crate tango;
extern crate walkdir;

fn main() {
    tango::process_root().unwrap();

    // We must run pandoc's processing after tango since tango may
    // generate `.md` files that we want to process.
    run_pandoc();
}

fn run_pandoc() {
    use pandoc::PandocOption as O;
    use pandoc::OutputFormat as F;

    let mut pandoc = pandoc::new();

    fn md_entry<E: ::std::fmt::Debug>(e: &Result<walkdir::DirEntry, E>) -> bool {
        let file_name = e.as_ref().unwrap().file_name().to_string_lossy();

        file_name.ends_with(".md") && file_name != "lib.md"
    }

    for entry in walkdir::WalkDir::new("src").into_iter().filter(|e| md_entry(e))
    {
        let entry = entry.unwrap();
        pandoc.add_input(entry.path());
    }
    pandoc.set_output(pandoc::OutputKind::File("../two-years-of-rust.html".to_owned()));
    pandoc.set_output_format(F::Revealjs, vec![]);
    // pandoc.set_output_format(F::Html, vec![]);
    for opt in vec![
        O::Standalone,
        O::Var("theme".to_owned(), Some("mozilla-sandstone".to_owned())),
        O::Var("center".to_owned(), Some("false".to_owned())),
        // O::Var("slideNumber".to_owned(), Some("c / t".to_owned())),

        O::Css("slide-style.css".to_owned()),
        O::Css("code-style.css".to_owned()),
        O::Css("fonts.css".to_owned()),

        // Setting this explicitly will hopefully avoid the whole
        // document going beserk if I happen to follow an H1 by content
        // with no intervening H2.
        O::SlideLevel(2)
    ] {
        pandoc.add_option(opt);
    }

    pandoc.execute().unwrap();
}
