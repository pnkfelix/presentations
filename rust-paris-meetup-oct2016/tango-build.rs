extern crate tango;
extern crate pandoc;
extern crate pandoc_ast;
extern crate mon_artiste;
extern crate pandoc_filter;

fn main() {
    // println!("cargo:warning='tango says hello'");
    tango::process_root().unwrap();
    run_pandoc();
    run_java_examples();
}

fn run_java_examples() {
    std::process::Command::new("javac").args(&["-d", ".", "src/Examples.java"]).output().unwrap();
    std::process::Command::new("java").args(&["Examples"]).output().unwrap();
}

fn run_pandoc() {
    let mut pandoc = pandoc::new();
    pandoc.add_input("src/slides.md");
    pandoc.set_output("slides.html");
    pandoc.add_option(pandoc::PandocOption::Css("stripes.css".to_string()));
    pandoc.add_option(pandoc::PandocOption::Css("slide-style.css".to_string()));
    pandoc.add_option(pandoc::PandocOption::Css("code-style.css".to_string()));
    pandoc.set_output_format(pandoc::OutputFormat::Revealjs);
    pandoc.add_option(pandoc::PandocOption::Standalone);
    pandoc.add_option(pandoc::PandocOption::SlideLevel(2));
    pandoc.set_variable("theme", "mozilla-sandstone");
    // pandoc.set_variable("theme", "White");
    pandoc.set_variable("center", "false");
    pandoc.set_variable("slideNumber", "'c / t'");
    pandoc.add_filter(run_art_filter);
    pandoc.execute().unwrap();
}

use pandoc_ast::{Pandoc, Block};
fn run_art_filter(input: String) -> String {
    pandoc_ast::filter(input, |mut ast: Pandoc| -> Pandoc {
        use pandoc_filter::PandocVisitor;
        RunMonArtiste.visit_pandoc(&mut ast);
        ast
    })
}

struct RunMonArtiste;
impl pandoc_filter::PandocVisitor for RunMonArtiste {
    fn visit_block(&mut self, block: &mut Block) {
        let new_content = match *block {
            Block::CodeBlock(ref mut attr, ref mut content) => {
                let (ref id, ref classes, ref _key_vals) = *attr;
                if classes.iter().find(|x| x == &"art").is_some() {
                    // println!("cargo:warning=visiting block of art");
                    run_mon_artiste(id, content)
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

fn run_mon_artiste(id: &str, content: &str) -> String {
    use mon_artiste::grid::{Grid};
    use mon_artiste::svg::{IntoElement};
    use mon_artiste::render::svg::{SvgRender};
    use mon_artiste::render::{RenderS};
    let r = SvgRender {
        x_scale: 10, y_scale: 16,
        font_family: "Menlo".to_string(), font_size: 16,
        show_gridlines: false,
        infer_rect_elements: true,
        name: (if id == "" { "rustfest" } else { id }).to_string(),
    };
    let s = content.parse::<Grid>().unwrap().into_scene(&Default::default());
    let elem = r.render_s(&s);
    elem.into_element().to_string()
}
