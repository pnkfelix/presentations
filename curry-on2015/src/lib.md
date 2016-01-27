# curry-on 2015 presentation materials

## good old lib.md

Just as in my [whistler tutorial][cyot whistler],
I am structuring this slide deck
so that the `lib.md` and `mod.md` files are ignored
but every other `.md` file is included in the pandoc output.

[cyot whistler]: https://github.com/pnkfelix/cyot/blob/master/src/tutorial/whistler_rust_intro/mod.md

So, I will use this space for meta-text with notes on how this code
and the corresponding slides are structured.


Ugh, I once again resorted to including the `thread::scoped`
code, "just in case"

```rust
#![feature(test)]
```

Here are any `extern crate` declarations that I happen
to be referencing in the presentation.

```rust
extern crate dispatch;
extern crate rayon;
```

Next, the module hierarchy for the presentation itself.

```rust
mod a00_title;
mod a01_start;
mod k00_concurrency;
mod l00_test_ligatures;
mod z00_conclusion;
mod z99_thanks;
```

Here is a unit test:

```rust
#[test]
fn it_works() {
    println!("Hello World");
}
```

## Runnable presentation

I try to follow a protocol of testing as much of the presented code as I can.

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
