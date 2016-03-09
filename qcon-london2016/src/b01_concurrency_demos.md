# Sharing Work: Parallelism / Concurrency {.center}

## Threading APIs (plural!)

 * `std::thread`

 * `dispatch` : OS X-specific "Grand Central Dispatch"

 * `crossbeam` : Lock-Free Abstractions, Scoped "Must-be" Concurrency

 * `rayon` : Scoped Fork-join "Maybe" Parallelism (inspired by Cilk)

## std::thread { data-transition="fade-out" }

```rust
fn concurrent_web_fetch() -> Vec<::std::thread::JoinHandle<()>> {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method


    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
        "http://imgur.com", "http://mozilla.org"];
    let mut handles = Vec::new();
    for site_ref in sites {
        let site = *site_ref;
        let handle = ::std::thread::spawn(move || {
            // block code put in closure: ~~~~~~~
            let client = Client::new();

            let res = client.get(site).send().unwrap();
            assert_eq!(res.status, hyper::Ok);
            let char_count = res.chars().count();
            println!("site: {} chars: {}", site, char_count);
        });

        handles.push(handle);
    }

    return handles;
}
```

## dispatch { data-transition="fade-in" }

```rust
fn concurrent_gcd_fetch() -> Vec<::dispatch::Queue> {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method
    use dispatch::{Queue, QueueAttribute};

    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
        "http://imgur.com", "http://mozilla.org"];
    let mut queues = Vec::new();
    for site_ref in sites {
        let site = *site_ref;
        let q = Queue::create("qcon2016", QueueAttribute::Serial);
        q.async(move || {
            let client = Client::new();

            let res = client.get(site).send().unwrap();
            assert_eq!(res.status, hyper::Ok);
            let char_count = res.chars().count();
            println!("site: {} chars: {}", site, char_count);
        });

        queues.push(q);
    }

    return queues;
}
```

<!--
```rust
#[test]
fn gcd_web_fetch() {
    println!("gcd_web_fetch");
    for q in concurrent_gcd_fetch() { q.sync(|| {}); }
    panic!("want to see output");
}
```
-->

## crossbeam {.big_text .center}

### lock-free data structures

### scoped threading abstraction

## lock-free data structures {.big_text .center}

## `crossbeam` MPSC benchmark

mean ns/msg
(2 producers, 1 consumer; msg count 10e6; 1G heap)

<table>
<tr>
   <td><div style="height: 108px;" class="bar">108ns</div></td><!--
--><td><div style="height:  98px;" class="bar">98ns</div></td><!--
--><td><div style="height:  53px;" class="bar">53ns</div></td><!--
--><td><div style="height: 461px;" class="bar">461ns</div></td><!--
--><td><div style="height: 192px;" class="bar">192ns</div></td>
</tr>
<tr>
<td>Rust channel</td>
<td>`crossbeam` MSQ</td>
<td>`crossbeam` SegQueue</td>
<td>Scala MSQ</td>
<td>Java ConcurrentLinkedQueue</td>
</tr>
</table>

## `crossbeam` MPMC benchmark

mean ns/msg (2 producers, 2 consumers; msg count 10e6; 1G heap)

<table>
<tr>
   <td><div style="height:   0px;" class="bar"></div></td><!--
--><td><div style="height: 102px;" class="bar">102ns</div></td><!--
--><td><div style="height:  58px;" class="bar"> 58ns</div></td><!--
--><td><div style="height: 239px;" class="bar">239ns</div></td><!--
--><td><div style="height: 204px;" class="bar">204ns</div></td>
</tr>
<tr>
<td>Rust channel (N/A)</td>
<td>`crossbeam` MSQ</td>
<td>`crossbeam` SegQueue</td>
<td>Scala MSQ</td>
<td>Java ConcurrentLinkedQueue</td>
</tr>
</table>

See "Lock-freedom without garbage collection"
 `https://aturon.github.io/blog/2015/08/27/epoch/`


## scoped threading?

`std::thead` does not allow sharing stack-local data

``` {.rust .compile_error}
fn std_thread_fail() {
    let array: [u32; 3] = [1, 2, 3];

    for i in &array {
        ::std::thread::spawn(|| {
            println!("element: {}", i);
        });
    }
}
```

. . .

```
error: `array` does not live long enough
```

## `crossbeam` scoped threading

```rust
fn crossbeam_demo() {
    let array = [1, 2, 3];

    ::crossbeam::scope(|scope| {
        for i in &array {
            scope.spawn(move || {
                println!("element: {}", i);
            });
        }
    });
}
```

. . .

`::crossbeam::scope` enforces parent thread joins on
all spawned children before returning

 * ensures that it is sound for children to access local references
   passed into them.

## crossbeam `scope`: "must-be concurrency"  {.big_text .center}

Each `scope.spawn(..)` invocation fires up a fresh thread

(Literally just a wrapper around `std::thread`)

## `rayon`: "maybe parallelism" {.big_text .center}

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

## Rayon's Rule

> the decision of whether or not to use parallel threads is
> made dynamically, based on whether idle cores are available

. . .

i.e., solely for offloading work, *not* for when concurrent operation
is necessary for correctness

. . .

(uses work-stealing under the hood to distribute work among a fixed
set of threads)

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


## Big Idea {.big_text .center}

3rd parties identify (and provide) *new abstractions*
for concurrency and parallelism unanticipated in std lib.
