# Sharing Code: Cargo {.center}

## Cargo

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

The compiler ensures that you never mistakenly pass a struct defined
in version 2 of library X to a function that is expecting the struct
to match that of X version 1.

[Semantic Versioning]: http://semver.org/
