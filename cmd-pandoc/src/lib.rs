use std::convert;
use std::env;
//use std::error::Error as ErrorTrait;
use std::fs;
use std::io;
use std::ops::Deref;
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

#[derive(Copy, Clone, Debug)]
pub enum TrackChanges { Accept, Reject, All }

impl TrackChanges {
    fn display(&self) -> &'static str {
        match *self {
            TrackChanges::Accept => "accept",
            TrackChanges::Reject => "reject",
            TrackChanges::All    => "all",
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum EmailObfuscation { None, Javascript, References }

impl EmailObfuscation {
    fn display(&self) -> &'static str {
        match *self {
            EmailObfuscation::None => "none",
            EmailObfuscation::Javascript => "javascript",
            EmailObfuscation::References => "references",
        }
    }
}

pub type URL = str;

#[derive(Copy, Clone, Debug)]
pub enum PandocOption<'a> {
    To(OutputFormat),                   // -t FORMAT  --to=FORMAT
    DataDir(&'a Path),                   // --data-dir=DIRECTORY
    Strict,                             // --strict
    ParseRaw,                           // -R --parse-raw
    Smart,                              // -S --smart
    OldDashes,                          // --old-dashes
    BaseHeaderLevel(u32),               // --base-header-level=NUMBER
    IndentedCodeClasses(&'a str),       // --indented-code-classes=STRING
    Filter(&'a Path),                   // -F PROGRAM --filter=PROGRAM
    Normalize,                          // --normalize
    PreserveTabs,                       // -p --preserve-tabs
    TabStop(u32),                       // --tab-stop=NUMBER
    TrackChanges(TrackChanges),         // --track-changes=accept|reject|all
    ExtractMedia(&'a Path),             // --extract-media=PATH
    Standalone,                         // -s --standalone
    Template(&'a Path),                 // --template=FILENAME
    Meta(&'a str, Option<&'a str>),     // -M KEY[:VALUE] --metadata=KEY[:VALUE]
    Var(&'a str, Option<&'a str>),      // -V KEY[:VALUE] --variable=KEY[:VALUE]
    PrintDefaultTemplate(&'a str),      // -D FORMAT --print-default-template=FORMAT
    PrintDefaultDataFile(&'a Path),     // --print-default-data-file=FILE
    NoWrap,                             // --no-wrap
    Columns(u32),                       // --columns=NUMBER
    TableOfContents,                    // --toc, --table-of-contents
    TableOfContentsDepth(u32),          // --toc-depth=NUMBER
    NoHighlight,                        // --no-highlight
    HighlightStyle(&'a str),            // --highlight-style=STYLE
    IncludeInHeader(&'a Path),          // -H FILENAME --include-in-header=FILENAME
    IncludeBeforeBody(&'a Path),        // -B FILENAME --include-before-body=FILENAME
    IncludeAfterBody(&'a Path),         // -A FILENAME --include-after-body=FILENAME
    SelfContained,                      // --self-contained
    Offline,                            // --offline
    Html5,                              // -5 --html5
    HtmlQTags,                          // --html-q-tags
    Ascii,                              // --ascii
    ReferenceLinks,                     // --reference-links
    AtxHeaders,                         // --atx-headers
    Chapters,                           // --chapters
    NumberSections,                     // -N --number-sections
    NumberOffset(&'a [u32]),            // --number-offset=NUMBERS
    NoTexLigatures,                     // --no-tex-ligatures
    Listings,                           // --listings
    Incremental,                        // -i --incremental
    SlideLevel(u32),                    // --slide-level=NUMBER
    SectionDivs,                        // --section-divs
    DefaultImageExtension(&'a str),     // --default-image-extension=extension
    EmailObfuscation(EmailObfuscation), // --email-obfuscation=none|javascript|references
    IdPrefix(&'a str),                  // --id-prefix=STRING
    TitlePrefix(&'a str),               // -T STRING --title-prefix=STRING
    Css(&'a URL),                       // -c URL --css=URL
    ReferenceOdt(&'a Path),             // --reference-odt=FILENAME
    ReferenceDocx(&'a Path),            // --reference-docx=FILENAME
    EpubStylesheet(&'a Path),           // --epub-stylesheet=FILENAME
    EpubCoverImage(&'a Path),           // --epub-cover-image=FILENAME
    EpubMetadata(&'a Path),             // --epub-metadata=FILENAME
    EpubEmbedFont(&'a Path),            // --epub-embed-font=FILE
    EpubChapterLevel(u32),              // --epub-chapter-level=NUMBER
    LatexEngine(&'a Path),              // --latex-engine=PROGRAM
    LatexEngineOpt(&'a str),            // --latex-engine-opt=STRING
    Bibliography(&'a Path),             // --bibliography=FILE
    Csl(&'a Path),                      // --csl=FILE
    CitationAbbreviations(&'a Path),    // --citation-abbreviations=FILE
    Natbib,                             // --natbib
    Biblatex,                           // --biblatex
    LatexMathML(Option<&'a URL>),       // -m[URL] --latexmathml[=URL], --asciimathml[=URL]
    AsciiMathML(Option<&'a URL>),       // --asciimathml[=URL]
    MathML(Option<&'a URL>),            // --mathml[=URL]
    MimeTex(Option<&'a URL>),           // --mimetex[=URL]
    WebTex(Option<&'a URL>),            // --webtex[=URL]
    JsMath(Option<&'a URL>),            // --jsmath[=URL]
    MathJax(Option<&'a URL>),           // --mathjax[=URL]
    Katex(Option<&'a URL>),             // --katex[=URL]
    KatexStylesheet(&'a URL),   // --katex-stylesheet=URL
    GladTex,                            // -gladtex
    Trace,                              // --trace
    DumpArgs,                           // --dump-args
    IgnoreArgs,                         // --ignore-args
    Verbose,                            // --verbose
}

#[derive(Copy, Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum OutputFormat {
    asciidoc, beamer, commonmark, context, docbook, docx, dokuwiki,
    dzslides, epub, epub3, fb2, haddock, html, html5, icml, json,
    latex, man, markdown, markdown_github, markdown_mmd,
    markdown_phpextra, markdown_strict, mediawiki, native, odt,
    opendocument, opml, org, plain, revealjs, rst, rtf, s5,
    slideous, slidy, texinfo, textile
}

macro_rules! cases {
    ($x:expr => $($id:ident),*) => {
        match $x {
            $($id => stringify!($id)),*
        }
    }
}

impl Deref for OutputFormat {
    type Target = str;

    fn deref(&self) -> &str {
        use OutputFormat::*;
        cases!(*self => asciidoc, beamer, commonmark, context, docbook, docx, dokuwiki,
               dzslides, epub, epub3, fb2, haddock, html, html5, icml, json,
               latex, man, markdown, markdown_github, markdown_mmd,
               markdown_phpextra, markdown_strict, mediawiki, native, odt,
               opendocument, opml, org, plain, revealjs, rst, rtf, s5,
               slideous, slidy, texinfo, textile)
    }
}

impl<'a> PandocOption<'a> {
    fn apply<'c>(&self, pandoc: &'c mut Command) -> &'c mut Command {
        use PandocOption::*;
        match *self {

            NumberOffset(nums)       => {
                let nums = nums.iter()
                    .fold(String::new(),
                          |b, n| {
                              if b.len() == 0 {
                                  format!("{}", n)
                              } else {
                                  format!("{}, {}", b, n)
                              }
                          });
                pandoc.args(&[&format!("--number-offset={}", nums)])
            }

            To(ref f)                => pandoc.args(&["-t", f]),
            DataDir(dir)             => pandoc.args(&[&format!("--data-dir={}", dir.display())]),
            Strict                   => pandoc.args(&["--strict"]),
            ParseRaw                 => pandoc.args(&["--parse-raw"]),
            Smart                    => pandoc.args(&["--smart"]),
            OldDashes                => pandoc.args(&["--old-dashes"]),
            BaseHeaderLevel(n)       => pandoc.args(&[&format!("--base-header-level={}", n)]),
            IndentedCodeClasses(s)   => pandoc.args(&[&format!("--indented-code-classes={}", s)]),
            Filter(program)          => pandoc.args(&[&format!("--filter={}", program.display())]),
            Normalize                => pandoc.args(&["--normalize"]),
            PreserveTabs             => pandoc.args(&["--preserve-tabs"]),
            TabStop(n)               => pandoc.args(&[&format!("--tab-stop={}", n)]),
            TrackChanges(ref v)      => pandoc.args(&[&format!("--track-changes={}", v.display())]),
            ExtractMedia(p)          => pandoc.args(&[&format!("--extract-media={}", p.display())]),
            Standalone               => pandoc.args(&["--standalone"]),
            Template(p)              => pandoc.args(&[&format!("--template={}", p.display())]),
            Meta(k, Some(v))         => pandoc.args(&["-M", &format!("{}:{}", k, v)]),
            Meta(k, None)            => pandoc.args(&["-M", k]),
            Var(k, Some(v))          => pandoc.args(&["-V", &format!("{}:{}", k, v)]),
            Var(k, None)             => pandoc.args(&["-V", k]),
            PrintDefaultTemplate(f)  => pandoc.args(&[&format!("--print-default-template={}", f)]),
            PrintDefaultDataFile(f)  => pandoc.args(&[&format!("--print-default-data-file={}", f.display())]),
            NoWrap                   => pandoc.args(&["--no-wrap"]),
            Columns(n)               => pandoc.args(&[&format!("--columns={}", n)]),
            TableOfContents          => pandoc.args(&["--table-of-contents"]),
            TableOfContentsDepth(d)  => pandoc.args(&[&format!("--toc-depth={}", d)]),
            NoHighlight              => pandoc.args(&["--no-highlight"]),
            HighlightStyle(s)        => pandoc.args(&[&format!("--highlight-style={}", s)]),
            IncludeInHeader(p)       => pandoc.args(&[&format!("--include-in-header={}", p.display())]),
            IncludeBeforeBody(p)     => pandoc.args(&[&format!("--include-before-body={}", p.display())]),
            IncludeAfterBody(p)      => pandoc.args(&[&format!("--include-after-body={}", p.display())]),
            SelfContained            => pandoc.args(&["--self-contained"]),
            Offline                  => pandoc.args(&["--offline"]),
            Html5                    => pandoc.args(&["--html5"]),
            HtmlQTags                => pandoc.args(&["--html-q-tags"]),
            Ascii                    => pandoc.args(&["--ascii"]),
            ReferenceLinks           => pandoc.args(&["--reference-links"]),
            AtxHeaders               => pandoc.args(&["--atx-headers"]),
            Chapters                 => pandoc.args(&["--chapters"]),
            NumberSections           => pandoc.args(&["--number-sections"]),
            NoTexLigatures           => pandoc.args(&["--no-tex-ligatures"]),
            Listings                 => pandoc.args(&["--listings"]),
            Incremental              => pandoc.args(&["--incremental"]),
            SlideLevel(n)            => pandoc.args(&[format!("--slide-level={}", n)]),
            SectionDivs              => pandoc.args(&["--section-divs"]),
            DefaultImageExtension(s) => pandoc.args(&[format!("--default-image-extension={}", s)]),
            EmailObfuscation(o)      => pandoc.args(&[format!("--email-obfuscation={}", o.display())]),
            IdPrefix(s)              => pandoc.args(&[format!("--id-prefix={}", s)]),
            TitlePrefix(s)           => pandoc.args(&[format!("--title-prefix={}", s)]),
            Css(url)                 => pandoc.args(&[format!("--css={}", url)]),
            ReferenceOdt(file)       => pandoc.args(&[format!("--reference-odt={}", file.display())]),
            ReferenceDocx(file)      => pandoc.args(&[&format!("--reference-docx={}", file.display())]),
            EpubStylesheet(file)     => pandoc.args(&[&format!("--epub-stylesheet={}", file.display())]),
            EpubCoverImage(file)     => pandoc.args(&[&format!("--epub-cover-image={}", file.display())]),
            EpubMetadata(file)       => pandoc.args(&[&format!("--epub-metadata={}", file.display())]),
            EpubEmbedFont(file)      => pandoc.args(&[&format!("--epub-embed-font={}", file.display())]),
            EpubChapterLevel(num)    => pandoc.args(&[&format!("--epub-chapter-level={}", num)]),
            LatexEngine(program)     => pandoc.args(&[&format!("--latex-engine={}", program.display())]),
            LatexEngineOpt(s)        => pandoc.args(&[&format!("--latex-engine-opt={}", s)]),
            Bibliography(file)       => pandoc.args(&[&format!("--bibliography={}", file.display())]),
            Csl(file)                => pandoc.args(&[&format!("--csl={}", file.display())]),
            CitationAbbreviations(f) => pandoc.args(&[&format!("--citation-abbreviations={}", f.display())]),
            Natbib                   => pandoc.args(&["--natbib"]),
            Biblatex                 => pandoc.args(&["--biblatex"]),
            LatexMathML(Some(url))   => pandoc.args(&[&format!("--latexmathml={}", url)]),
            AsciiMathML(Some(url))   => pandoc.args(&[&format!("--asciimathml={}", url)]),
            MathML(Some(url))        => pandoc.args(&[&format!("--mathml={}", url)]),
            MimeTex(Some(url))       => pandoc.args(&[&format!("--mimetex={}", url)]),
            WebTex(Some(url))        => pandoc.args(&[&format!("--webtex={}", url)]),
            JsMath(Some(url))        => pandoc.args(&[&format!("--jsmath={}", url)]),
            MathJax(Some(url))       => pandoc.args(&[&format!("--mathjax={}", url)]),
            Katex(Some(url))         => pandoc.args(&[&format!("--katex={}", url)]),
            LatexMathML(None)        => pandoc.args(&["--latexmathml"]),
            AsciiMathML(None)        => pandoc.args(&["--asciimathml"]),
            MathML(None)             => pandoc.args(&["--mathml"]),
            MimeTex(None)            => pandoc.args(&["--mimetex["]),
            WebTex(None)             => pandoc.args(&["--webtex["]),
            JsMath(None)             => pandoc.args(&["--jsmath["]),
            MathJax(None)            => pandoc.args(&["--mathjax["]),
            Katex(None)              => pandoc.args(&["--katex["]),
            KatexStylesheet(url)     => pandoc.args(&[&format!("--katex-stylesheet={}", url)]),
            GladTex                  => pandoc.args(&["--gladtex"]),
            Trace                    => pandoc.args(&["--trace"]),
            DumpArgs                 => pandoc.args(&["--dump-args"]),
            IgnoreArgs               => pandoc.args(&["--ignore-args"]),
            Verbose                  => pandoc.args(&["--verbose"]),
        }
    }
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

pub fn run_pandoc(src_dir: &str, target_name: &str, opts: &[PandocOption]) -> Result<()> {
    let src_dir_path = &format!("src/{}", src_dir);
    let mut src_paths = Vec::new();
    for entry in try!(read_dir(src_dir_path)) {
        let entry = try!(entry);
        if is_skipped_md(&entry) { continue; }
        if let Some("md") = entry.path().extension().and_then(|p|p.to_str()) {
            src_paths.push(entry.path());
        }
    }
    let tgt_path = &format!("{}.html", target_name);
    let mut pandoc = Command::new("pandoc");
    for o in opts {
        o.apply(&mut pandoc);
    }
    pandoc.args(&["-o", tgt_path]);
    src_paths.sort();
    for p in src_paths {
        pandoc.arg(p);
    }

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

fn is_skipped_md(entry: &fs::DirEntry) -> bool {
    {
        match entry.path().file_name().and_then(|p|p.to_str()) {
            Some("mod.md") | Some("lib.md") => true,
            _ => false,
        }
    }
}


#[test]
fn it_works() {
}
