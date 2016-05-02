pnkfelix/presentations
=========================

Source for slideshow presentations, mostly of Rust, at various conferences.

The source for particular presentations tends to be held in individual branches.

The *deployed* presentations are all collected together on the
`gh-pages` branch.

The index is hosted, at least in principle, at:
http://pnkfelix.github.io/presentations/

If you have having problems building (via (`cargo build`) out of the box (especially on the `qcon-london-2016` branch), try looking at the following:

 * http://stackoverflow.com/questions/34612395/openssl-crate-fails-compilation-on-mac-os-x-10-11

----

If the generated page is not rendering as a slideshow, you may need to do one or more of the following:

 * Run `git submodule update --init` from the root of the checkout (to pull down the `moz-reveal` and `reveal.js` submodules that hold CSS referenced by the generated html).

 * You may need a sufficiently new version of `pandoc`. From what I have seen, version 1.12 is too old, while version 1.14 works, as does the current version as of this writing, version 1.17.03.
