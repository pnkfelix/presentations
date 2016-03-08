Features:

 * I want the `test` crate which needs the `test` feature.

 * I want to use the `chars` method of the `std::io::Read` trait,
   which needs the `io` feature.

```rust
#![feature(test)]
#![feature(io)] // ugh what a shame
```

We use #[bench] which requires the `test` crate.

```rust
extern crate test;
```

This is to support my "little" mandelbrot demo.

```rust
extern crate piston_window;
```

This is to support demo'ing concurrent web fetch.

```rust
extern crate hyper;
```

As a prelude to rayon, I use `.foreach` on an iterator, which I grabbed from itertools.
```rust
extern crate itertools;
```

And of course we want to include rayon demos.

```rust
extern crate rayon;
```

We list all the source we want included as part of the build / test process here.

```rust
pub mod a00_title;
pub mod a01_opening;
pub mod a02_ownership;
pub mod a03_sharing;
pub mod a04_parallelism;
pub mod a05_cargo;
pub mod b01_concurrency_demos;
pub mod z99_closing;
```
