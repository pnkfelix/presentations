# Connected Devices Kickoff 2016 Presentation Materials

## good old lib.md

Just as in my [whistler tutorial][whistler] and my
[curry-on presentation][curry-on], I am structuring this slide
deck so that the `lib.md` and `mod.md` files are ignored but every
other `.md` file is included in the pandoc output.

[whistler]: https://github.com/pnkfelix/cyot/blob/master/src/tutorial/whistler_rust_intro/mod.md

[curry-on]: https://github.com/pnkfelix/presentations/blob/curry-on2015/curry-on2015/src/lib.md

In addition to the aforementioned [whistler][] and [curry-on][]
materials, much of the text here is also drawn from the
[Mozlando Rust tutorial][mozlando] material by nikomatsakis.

[mozlando]: http://smallcultfollowing.com/20151209/

So, how does this text work?

This markdown text that you are reading will be transcribed into a
Rust source file. The transcription is pretty simple-minded (much less
so than say "tangle" and "weave" that Literate WEB programs use), so
we need to start with the same things that you would need to start off
any crate: `#!` attributes and `extern crate` declarations.

We ignore all dead code warnings (and similar, like unused variable warnings), since so much of the presentation
consists of little functions that are not exported nor called.

```rust
#![allow(dead_code, unused_variables)]
```

We pull in Niko's `rayon` crate for slick (and short) demos
of parallel code.

```rust
extern crate rayon;
extern crate rand;
```
Next, the (shallow) module hierarchy for the presentation itself.

```rust
mod slides;

mod exercises;
```

Here is a unit test:

```rust
#[cfg(test)]
mod test {
    #[test]
    fn it_works() {
    }
}
```

## Runnable presentation

The contents of every code block that looks like:

```
    ```rust {optional block metadata here}
    /* code here */
    ```
```

should be transcribed by `tango` into rust code. If the file that it
is transcribed into is also part of the `mod`-hierarchy, then the code
will be part of the cargo build.

### Skipping compile-errors

The auto-transcription performed by `tango` gets tricky when it comes
to illustrating compile-errors in a presentation.

The easy work-arounds are:

 1. Leave any file that is expected to be uncompilable out of the
    module hierarchy.

    (Longer term it would perhaps be nice to figure out some way to
    still test that such files error at the point where expected,
    e.g. by running them through `rustc --pretty expanded` or
    something when they are intended to have a type-error but still
    remain parsable.)

 2. Do not use the ```` ```rust ```` form to fence the code block.
    You can instead use e.g. ```` ``` {.rust} ```` as the fence;
    `pandoc` will then render such a block just the same, but `tango`
    will not treat it as code to transcribe.

# Regarding accidental content

I am now forcing `slide-level=2` in the pandoc invocation.

This means that content that occurs
directly underneath a `h1` element (and not an `h2` element)
will not
cause the overall slide structure to be squished into a single column
in reveal.js.

* e.g., I'm talking about content like this text here:

  ```
  # H1 header

  occurs here
  ```


However, due to [pandoc issues], such content will instead be
silently dropped from the generated output.

[pandoc issues]: https://github.com/jgm/pandoc/issues/2265

So, be wary of including such content!
