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
