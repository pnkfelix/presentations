## Parallel <span style="visibility: hidden">and Safe</span> { data-transition="fade-out" }

[Rayon]: https://crates.io/crates/rayon/

```rust
use rayon;
fn parallel_qsort(vec: &mut [i32]) {
    if vec.len() <= 1 { return; }
    let pivot = vec[::rand::random(vec.len())];
    let mid = vec.partition(vec, pivot);
    let (less, greater) = vec.split_at_mut(mid);
    rayon::join(|| parallel_qsort(less),
                || parallel_qsort(greater)
    );
}
```

(Shout out to [Rayon][].)

. . .

## Parallel and Safe  { data-transition="fade-in" }

``` {.rust .compile_error}
use rayon;
fn parallel_qsort(vec: &mut [i32]) {
    if vec.len() <= 1 { return; }
    let pivot = vec[::rand::random(vec.len())];
    let mid = vec.partition(vec, pivot);
    let (less, greater) = vec.split_at_mut(mid);
    rayon::join(|| parallel_qsort(less),
                || parallel_qsort(less)
    );
}
```
