# Sharing Code: Cargo {.center}

## Sharing Code

`std::thread` is provided with std lib

. . .

But `crossbeam` and `rayon` are 3rd party

. . .

What is Rust's code distribution story?

## Cargo

`cargo` is really simple to use

. . .

```
cargo new     -- create a project
cargo test    -- run project's unit tests
cargo run     -- run binaries associated with project
cargo publish -- push project up to crates.io
```

. . .

Edit the associated `Cargo.toml` file to:

 * add dependencies
 * specify version / licensing info
 * conditionally compiled features
 * add build-time behaviors (e.g. code generation)

. . .

"What's this about publishing to `crates.io`?"

## crates.io

Open-source crate distribution site

Has every version of every crate

Cargo adheres to *semantic versioning* ("semver")

## Semver

<!--
From the cargo docs

> Cargo bakes in the concept of [Semantic Versioning][], so make sure you follow some basic rules:
>
> * Before you reach 1.0.0, anything goes.
> * After 1.0.0, only make breaking changes when you increment the major version. In Rust, breaking changes include adding fields to structs or variants to enums. Don’t break the build.
> * After 1.0.0, don’t add any new public API (no new pub anything) in tiny versions. Always increment the minor version if you add any new pub structs, traits, fields, types, functions, methods or anything else.
> * Use version numbers with three numeric parts such as 1.0.0 rather than 1.0.
-->

The use of [Semantic Versioning][] in `cargo` basically amounts to this:

[Semantic Versioning]: http://semver.org/

Major versions (MAJOR.minor.patch) are free to break whatever they want.

New public API's can be added with minor versions updates
(major.MINOR.patch), as long as they do not impose breaking
changes.

. . .

In Rust, breaking changes *includes* data-structure representation changes.

Adding fields to structs (or variants to enums) can cause their memory
 representation to change.

## Why major versions can include breaking changes

Cargo invokes the Rust compiler in a way that salts the symbols
exported by a compiled library.

This ends up allowing two distinct (major) versions of a library to be
used *simultaneously* in the same program.

This is important when pulling in third party libraries.

<!--
## Fixing versions

`cargo` generates a `Cargo.lock` file that tracks the versions you built the project with

Intent: application (i.e. final) crates should check their `Cargo.lock` into version control

 * Ensures that future build attempts will choose the *same* versions

However: library (i.e. intermediate) crates should *not* check their `Cargo.lock` into version control.

 * Instead, everyone should follow sem.ver.; then individual applications can mix different libraries
   into their final product, upgrading intermediate libraries as necessary

-->

## In Practice

* If you (*) follow the sem.ver. rules, then you do not usually have
  to think hard about the crates you (recursively) import.

   * "you" is really "you and all the crates you use"

&nbsp;

>- You may not believe me, but `cargo` is really simple to use
>- Coming from a C/C++ world, this feels like magic
   >- (probably feels like old hat for people used to package dependency managers)
