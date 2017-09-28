extern crate pandoc;
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
        let entry = e.as_ref().expect("dir-entry is not ok");
        let file_name = entry.file_name().to_string_lossy();
        let path = entry.path();

        // ignore everything in `bin` directory.
        for comp in path.components() {
            if let ::std::path::Component::Normal(s) = comp {
                if s == "bin" {
                    return false;
                }
            }
        }

        // otherwise, process everything that ends with `.md` except the `lib.md`/`main.md`.
        file_name.ends_with(".md") && file_name != "lib.md" && file_name != "main.md"
    }

    let mut saw_file = false;
    for entry in walkdir::WalkDir::new("src").into_iter().filter(|e| md_entry(e))
    {
        let entry = entry.expect("entry is not ok");
        pandoc.add_input(entry.path());
        saw_file = true;
    }

    if !saw_file {
        panic!("why didn't tango produce at least one .md file (apart from lib.md)?");
    }

    pandoc.set_output(pandoc::OutputKind::File("../rustfest-zurich-2017.html".to_owned()));
    pandoc.set_output_format(F::Revealjs, vec![]);
    // pandoc.set_output_format(F::Html, vec![]);
    for opt in vec![
        O::Standalone,

        O::IncludeInHeader("header.html".into()),

        O::Var("theme".to_owned(), Some("mozilla-darkgrey".to_owned())),
        O::Var("center".to_owned(), Some("true".to_owned())),
        O::Var("history".to_owned(), Some("true".to_owned())),
        // O::Var("slideNumber".to_owned(), Some("c / t".to_owned())),
        // O::Var("slideNumber".to_owned(), Some("true".to_owned())),

        O::Css("slide-style.css".to_owned()),
        O::Css("code-style-darkgrey.css".to_owned()),
        O::Css("fonts.css".to_owned()),

        // Setting this explicitly will hopefully avoid the whole
        // document going beserk if I happen to follow an H1 by content
        // with no intervening H2.
        O::SlideLevel(2)
    ] {
        pandoc.add_option(opt);
    }

    pandoc.execute().expect("pandoc execution was not ok");
}
