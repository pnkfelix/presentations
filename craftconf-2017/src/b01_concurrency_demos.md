# Sharing Work: Parallelism / Concurrency {.center}

FIXME: Add material on `Send` and `Sync`

## Threading APIs (plural!)

 * `std::thread`

 * `crossbeam` : Lock-Free Abstractions, Scoped "Must-be" Concurrency

 * `rayon` : Scoped Fork-join "Maybe" Parallelism (inspired by Cilk)

(Only the *first* comes with Rust out of the box)

## std::thread { data-transition="fade-out" }

<!--
```rust
fn messages() -> Vec<String> {
    let mut info: Vec<String> = Vec::new();
    for i in 0..10 {
        let even_or_odd = if i % 2 == 0 { "even" } else { "odd" };
        info.push(format!("{} is {}", i, even_or_odd));
    }
    info
}

#[test]
fn std_thread_demo() {
    let handles = spawn_them(messages());
    for h in handles { h.join(); }
}
use std;
use std::thread::JoinHandle;
```
-->

```rust
fn spawn_them(messages: Vec<String>) -> Vec<JoinHandle<usize>> {
    let mut handles = Vec::new();
    for mut msg in messages {
        handles.push(std::thread::spawn(move || {
            msg.push_str(", isn't that special?");
            return msg.len();
        }));
    }
    return handles;
}
```

```art
    parent    threadN      thread3    thread2    thread1

    [msg1,  ------------------------------------->|
     msg2,  --------------------------->|         |
     msg3,  ---------------->|          |         |
      ⋮                      |          |         |
     msgN]  --->|[tn]        |[t3]      |[t2]     |[t1]
      :         :            :          :         :
      |[parent] |            |          |         |
 handle3.join() |            |          |         |
      | <-------+----------- o          |         |
      :         |                       |         |
      |[paren2] |                       |         |
[parent]: stroke="blue"
[paren2]: stroke="blue"
[t1]: stroke="blue"
[t2]: stroke="blue"
[t3]: stroke="blue"
[tn]: stroke="blue"
```

>- Joining `JoinHandle<T>` returns value (`T`) from thread's closure (`|| { ... }`)

>- Handles move like other values (can even be sent between threads)

>- Compiler assumes nothing of extents (`threadK` may outlive `parent`)

## What if sole ownership does not suffice?

Example: two threads both must read from one
heap-allocated vector. That `Vec` must
survive as long as the threads.

## One approach: shared ownership

<!--
```rust
use std::rc::Rc;
use std::sync::Arc;
```
-->

```rust
fn demo_sharing() {
    let mut v = Vec::new();
    v.push("Hello"); v.push(" "); v.push("World!");

    // transfer vector `v` to heap-allocated (and ref-counted) storage
    let rc1 = Rc::new(v);
    let rc2 = rc1.clone(); // increments ref-count on heap-alloc'd value
}
```

```art

+-------------+
|             |    r .--------.
|   rc1 --------=-+->|   2    |
|             |   |  +--------+
|             |   |  |        |
|   rc2 --------=-'  |    -=--+-> "Hello"
|             |      |        |
|             |      |    -=--+-> " "
|             |      |        |
|             |      |    -=--+-> "World!"
|             |      |        |
|             |      '--------'
     . . .
|             |
+-------------+
```

## Problem solved?

``` {.rust .compile_error}
fn failed_sharing_cross_threads() {
    let mut v = Vec::new();
    v.push("Hello"); v.push(" "); v.push("World!");

    // transfer vector `v` to heap-allocated (and ref-counted) storage
    let rc1 = Rc::new(v);
    let rc2 = rc1.clone(); // increments ref-count on heap-alloc'd value

    let h1 = std::thread::spawn(move || { rc1[0].len() });
    let h2 = std::thread::spawn(move || { rc2[2].len() });

    h1.join(); h2.join();
}
```

. . .

```
error: the trait bound `Rc<Vec<&str>>: Send` is not satisfied in `[closure]`
   --> src/b01_concurrency_demos.rs:126:14
    |
126 |     let h1 = std::thread::spawn(move || { rc1[0].len() });
    |              ^^^^^^^^^^^^^^^^^^ within `[closure]`, the trait `Send` is
    |                                 not implemented for `Rc<Vec<&str>>`
    |
    = note: `Rc<Vec<&str>>` cannot be sent between threads safely
    = note: required because it appears within the type `[closure]`
    = note: required by `std::thread::spawn`
```

## Issue

`Rc<T>` uses *non-atomic* updates for ref-count maintenance

Easily fixed: if you need to share ownership across threads, then use
`Arc<T>` instead.

```rust
fn demo_sharing_cross_threads() {
    let mut v = Vec::new();
    v.push("Hello"); v.push(" "); v.push("World!");

    // transfer vector `v` to heap-allocated (and ref-counted) storage
    let arc1 = Arc::new(v);
    let arc2 = arc1.clone(); // increments ref-count on heap-alloc'd value

    let h1 = std::thread::spawn(move || { arc1[0].len() });
    let h2 = std::thread::spawn(move || { arc2[2].len() });

    h1.join(); h2.join();
}
```

## Illustrating `Arc`

```art
    parent       thread1    thread2
+-------------+     |[t1a]     |[t2b]
|             |     |          |           r .-----------.
|  (arc1) -------> arc1 -------+--------=-+->| atomic(2) |
|             |     |          |          |  +-----------+
|             |     |          |          |  |           |
|  (arc2) ----------+--------> arc2 ----=-'  |    -=-----+-> "Hello"
|             |     |          |             |           |
|             |     |          |             |    -=-----+-> " "
|             |     |          |             |           |
|             |     |          |             |    -=-----+-> "World!"
|             |     |          |             |           |
|             |     |          |             '-----------'
     . . .          |          |
|             |     |          |
+-------------+     |[t1b]     |[t2b]
[t1a]: stroke="blue"
[t2a]: stroke="blue"
[t1b]: stroke="blue"
[t2b]: stroke="blue"
```


## More general idea {.left_align}

* `Rc<T>` vs `Arc<T>` is (an important) detail

* Fundamental *concept* is deeper

* Traits `Send` and `Sync` control cross-thread capabilities

>- `T: Send` implies ownership of `T` can be tranferred across threads

>- `T: Sync` implies a reference to `T` (e.g. `&T`, `Arc<T>`)
   can be shared across threads

>- `Send` enables Message Passing style concurrency, via channels

>- `Sync` enables Shared State style concurrency; only `T` with synchronized access is allowed

## Any other solutions to this besides shared ownership? {.center}

. . .

Yes; the best ones are in crates outside Rust stdlib.

## crossbeam {.big_text .center}

* provides lock-free data structures

* and a scoped threading abstraction

* upholds Rust's safety (data-race freedom) guarantees

## scoped threading?

`std::thead` does not allow sharing stack-local data

``` {.rust .compile_error}
fn std_thread_fail() {
    let array: [String; 3] = [format!("hello"), format!(" "), format!("world")];

    for string_ref in &array {
        std::thread::spawn(move || {
            let r: &String = string_ref;
            println!("element: {}", r);
        });
    }
 } // end of `array` scope; it is popped and strings are deallocated
```

. . .

```
error: `array` does not live long enough
  --> src/b01_concurrency_demos.rs:92:24
   |
92 |     for string_ref in &array {
   |                        ^^^^^ does not live long enough
...
98 |  } // end of `array` scope; it is popped and strings are deallocated
   |  - borrowed value only lives until here
   |
   = note: borrowed value must be valid for the static lifetime...
```

## `crossbeam` scoped threading

```rust
fn crossbeam_demo() {
    let array: [String; 3] = [format!("hello"), format!(" "), format!("world")];

    ::crossbeam::scope(|scope| {
        for string_ref in &array {
            let r: &String = string_ref;
            scope.spawn(move || {
                println!("element: {}", r);
            });
        }
    });
}
```

. . .

```art
parent    thread3    thread2    thread1

 [str1,<-----------------=-------- |
  str2,<-----=---------- |         |[t1]
  str3]<--=- |           |[t2]     |
   |         |[t3]       |         |
   | <------ o           |         |
   | <------------------ o         |
   | <---------------------------- o
   |
   |[p]
[p]: stroke="blue"
[t1]: stroke="blue"
[t2]: stroke="blue"
[t3]: stroke="blue"
```

`::crossbeam::scope` enforces parent thread joins on
all spawned children before returning

 * ensures child threads can soundly access local refs they receive

## crossbeam `scope`: "must-be concurrency"  {.big_text .center}

Each `scope.spawn(..)` invocation fires up a fresh thread

(Literally just a wrapper around `std::thread`)

## crossbeam `scope`: "must-be concurrency"  {.big_text .center}

### vs `rayon`: "may-be parallelism" {.fragment .big_text .center}

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

## `rayon` demo 2: quicksort  { data-transition="fade" }

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

```art
quick_sort            recur_lo         recur_hi
    |[q1]
    |
    | [0⋯mid, mid⋯]
    |
    |  ├───┤  ├───┤    |[rl1]          |[rh1]
          :  [s] :     |               |
          |      '-----+-------------- |
          '----------- |               |
                       |               |
                       |               |
                       |               |
                       |               |
                       |               |
                       |               |
                       |               |
    | <--------------- o               |
    | <------------------------------- o
    |
    |[q2]
[q1]: stroke="blue"
[q2]: stroke="blue"
[s]: stroke="green"
[rl1]: stroke="blue"
[rh1]: stroke="blue"
[rl2]: stroke="blue"
[rh2]: stroke="blue"
[w]: stroke="blue"
[x]: stroke="blue"
[y]: stroke="blue"
[z]: stroke="blue"
```


## `rayon` demo 2: quicksort  { data-transition="fade" }

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

```art
quick_sort            recur_lo         recur_hi
    |[q1]
    |
    | [0⋯mid, mid⋯]
    |
    |  ├───┤  ├───┤    |[rl1]          |[rh1]
          :  [s] :     |               |
          |      '-----+-------------- |
          '----------- |               |
                       |   |    |      |   |    |
                        -=-+--= |       -=-+--= |
                        -= |    |       -= |    |
                           |    |          |    |
                           o[w] o[x]       o[y] o[z]
                       |               |
                       |[rl2]          |[rh2]
    | <--------------- o               |
    | <------------------------------- o
    |
    |[q2]
[q1]: stroke="blue"
[q2]: stroke="blue"
[s]: stroke="green"
[rl1]: stroke="blue"
[rh1]: stroke="blue"
[rl2]: stroke="blue"
[rh2]: stroke="blue"
[w]: stroke="blue"
[x]: stroke="blue"
[y]: stroke="blue"
[z]: stroke="blue"
```

. . .

"Recursion is simple", right (?)

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
