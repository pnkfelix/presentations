# Sharing Work: Parallelism / Concurrency {.center}

## Big Idea {.big_text .center}

3rd parties identify (and provide) *new abstractions*
for (safe) concurrency and parallelism unanticipated in std lib.

## Example: `rayon`'s scoped parallelism {.big_text .center}

## `rayon` demo 1: map reduce

<!--
```rust
struct Store;
struct Item;
type Groceries<'a> = &'a [Item];
use rayon;
use rayon::prelude::*;
impl Store {
    fn compute_price(&self, g: Groceries) -> u32 { unimplemented!() }
}
```
-->

### Sequential

```rust
fn demo_map_reduce_seq(stores: &[Store], list: Groceries) -> u32 {
    let total_price = stores.iter()
                            .map(|store| store.compute_price(&list))
                            .sum();
    return total_price;
}
```

### Parallel (*potentially*)

```rust
fn demo_map_reduce_par(stores: &[Store], list: Groceries) -> u32 {
    let total_price = stores.par_iter()
                            .map(|store| store.compute_price(&list))
                            .sum();
    return total_price;
}
```

## `rayon` demo 2: quicksort  { data-transition="fade-out" }

```rust
fn quick_sort<T:PartialOrd+Send>(v: &mut [T]) {
    if v.len() > 1 {
        let mid = partition(v);
        let (lo, hi) = v.split_at_mut(mid);
        rayon::join(|| quick_sort(lo),
                    || quick_sort(hi));
    }
}
```
<!--
```rust
fn partition<T:PartialOrd+Send>(v: &mut [T]) -> usize {
    // see https://en.wikipedia.org/wiki/Quicksort#Lomuto_partition_scheme
    unimplemented!()
}
```
-->
``` {.rust}
fn partition<T:PartialOrd+Send>(v: &mut [T]) -> usize {
    // see https://en.wikipedia.org/wiki/
    //     Quicksort#Lomuto_partition_scheme
    ...
}
```

## `rayon` demo 3: buggy quicksort  { data-transition="fade-in" }

``` {.rust}
fn quick_sort<T:PartialOrd+Send>(v: &mut [T]) {
    if v.len() > 1 {
        let mid = partition(v);
        let (lo, hi) = v.split_at_mut(mid);
        rayon::join(|| quick_sort(lo),
                    || quick_sort(hi));
    }
}
```

``` {.rust .compile_error}
fn quick_sort<T:PartialOrd+Send>(v: &mut [T]) {
    if v.len() > 1 {
        let mid = partition(v);
        let (lo, hi) = v.split_at_mut(mid);
        rayon::join(|| quick_sort(lo),
                    || quick_sort(lo));
        //                        ~~ data race!
    }
}
```


(See blog post "Rayon: Data Parallelism in Rust" `bit.ly/1IZcku4`)

## Threading APIs (plural!)

 * `std::thread`

 * `dispatch` : OS X-specific "Grand Central Dispatch"

 * `crossbeam` : Lock-Free Abstractions, Scoped "Must-be" Concurrency

 * `rayon` : Scoped Fork-join "Maybe" Parallelism (inspired by Cilk)

(Only the *first* comes with Rust out of the box)
