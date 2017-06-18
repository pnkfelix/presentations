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
    pandoc.set_output(pandoc::OutputKind::File("../iwaco-2017.html".to_owned()));
    pandoc.set_output_format(F::Revealjs, vec![]);
    // pandoc.set_output_format(F::Html, vec![]);
    for opt in vec![
        O::Standalone,

        O::IncludeInHeader("header.html".into()),

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

    pandoc.add_filter(run_art_filter);

    pandoc.execute().unwrap();
}


use pandoc_ast::{Pandoc, Block, MutVisitor};
fn run_art_filter(input: String) -> String {
    pandoc_ast::filter(input, |mut ast: Pandoc| -> Pandoc {
        RunMonArtiste.walk_pandoc(&mut ast);
        ast
    })
}

struct RunMonArtiste;
impl MutVisitor for RunMonArtiste {
    fn visit_block(&mut self, block: &mut Block) {
        let new_content = match *block {
            Block::CodeBlock(ref mut attr, ref mut content) => {
                let (ref id, ref classes, ref _key_vals) = *attr;
                if classes.iter().find(|x| x == &"art").is_some() {
                    // println!("cargo:warning=visiting block of art");
                    run_mon_artist(id, content)
                } else {
                    return;
                }
            }
            _ => return,
        };

        // if we get here, then we *do* want to process the input,
        // replacing the block with a RawBlock holding the SVG
        // (aka html) output from mon-artiste.
        *block = Block::RawBlock(pandoc_ast::Format("html".to_string()),
                                 new_content);
    }
}

fn run_mon_artist(id: &str, content: &str) -> String {
    use mon_artist::grid::{Grid};
    use mon_artist::svg::{IntoElement};
    use mon_artist::render::svg::{SvgRender};
    use mon_artist::render::{RenderS};
    let new_rules = r#"
# Handle box characters in sensible way.
start          '├' (E) "─┬" draw "M {N} L {S} L {C}";
start          '┤' (W) "─┬" draw "M {N} L {S} L {C}";
step  "├─" (E) '┬' (E) "─┤" draw "L {C} L {S} L {C} L {E}";
step  "─┤" (W) '┬' (W) "├─" draw "L {C} L {S} L {C} L {W}";
end   "─┬" (E) '┤'          draw "L {C} L {S} L {N}";
end   "─┬" (W) '├'          draw "L {C} L {S} L {N}";
start          '┬' (S) '|' draw "M {W} L {E} L {C}";
start          '┴' (N) '|' draw "M {W} L {E} L {C}";
step   ANY ANY '|' (S) '┴' draw "";
step   ANY ANY '|' (N) '┬' draw "";
  end  '|' (N) '┬'         draw "L {C} L {W} L {E}";
  end  '|' (S) '┴'         draw "L {C} L {W} L {E}";
# end   '/'  (SW) 'V'        draw "L {SW} L {W} L {SW} L {S}";
# end   '\'  (SE) 'V'        draw "L {SE} L {E} L {SE} L {S}";
# end   '|'  (S)  'V'        draw "L {S}  L {W} L {S} L {E}";
# step  '/'  (SW) '/'  (SW) 'V'        draw "";
# step  '\'  (SE) '\'  (SE) 'V'        draw "";
# step  '|'  (S)  '|'  (S)  'V'        draw "";
"#;
    let format_table = format!("{}\n{}", new_rules, mon_artist::DEFAULT_RULES);
    let format_table = mon_artist::format::Table::from_lines(format_table.lines().map(|r|Ok(r.to_owned())));
    let s = content.parse::<Grid>().unwrap().into_scene(&format_table, Default::default());
    let r = SvgRender {
        x_scale: 10, y_scale: 16,
        font_family: "Menlo".to_string(), font_size: 16,
        show_gridlines: false,
        name: (if id == "" { "monartist-padl-2017" } else { id }).to_string(),
        infer_rect_elements: false,
        format_table: format_table,
    };
    let elem = r.render_s(&s);
    elem.into_element().to_string()
}
