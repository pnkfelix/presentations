% Hack Without Fear (with Rust)
% Felix Klock (`@pnkfelix`); February 2016
% FYI: Slides form *2D space*: <kbd class="key">&#x2420;</kbd> goes to next slide; <kbd class="key">&#x2190;</kbd> <kbd class="key">&#x2191;</kbd> <kbd class="key">&#x2192;</kbd> <kbd class="key">&#x2193;</kbd> navigate; <kbd class="key">&#x241B;</kbd> gives overview.

# Hack Without Fear {.center}

<div class="no_border logo">
![Rust Logo]
<div>

[Rust Logo]: Rust_programming_language_black_logo.svg

Parallel Systems Programming

----

## {.center .equation}

An "Equation"

--- ----------------------------------------
    Abstraction without overhead
    Memory safety without GC
  + Concurrency without data races
    ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
  = Hack without Fear
--- ----------------------------------------

. . .

What is meant by "Abstraction", "Memory Safety", "Concurrency"?

## Abstraction



## Memory Safety

## Concurrency

(Shout out to [Rayon][]. And to the Rust [playpen][rayon_demo_playpen].)

[Rayon]: https://crates.io/crates/rayon/

```rust
use rayon::par_iter::*;

struct Store;

fn min_val(stores: Vec<Store>) {
    let total_price = stores.par_iter();
}
```
[rayon_demo_playpen]: https://play.rust-lang.org/?code=use%20rayon%3A%3Apar_iter%3A%3A*%3B%0A%0Astruct%20Store%3B%0A%0Afn%20min_val%28stores%3A%20Vec%3CStore%3E%29%20%7B%0A%20%20%20%20let%20total_price%20%3D%20stores.par_iter%28%29%3B%0A%7D&version=nightly

## Next Slide
