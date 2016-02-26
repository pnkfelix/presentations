We use #[bench] which requires the `test` crate.

```rust
#![feature(test)]

extern crate test;
```

This is to support my "little" mandelbrot demo.

```rust
extern crate piston_window;
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
pub mod b01_concurrency_demos;
pub mod z99_closing;
```
