# Concurrency { .center }

## Concurrency

Rust's killer feature:

### Data-race freedom

built atop same foundation as memory safety

## Here's what one concurrency API looks like

## `thread::spawn`

```rust
pub fn main() {
    use std::thread;
    let al = "long lost pal";
    thread::spawn(move || {

        println!("i can be your {}", al);
    });

    println!("why am i soft in the middle");
    // Note: might exit before spawned thread gets chance to print
}
```

## channels for message passing

```rust
#[test] fn demo_channel() {
    fn fib(x: i64) -> (i64, i64) { // returns `(x, fib(x))`
        if x <= 1 { (x,1) } else { (x, fib(x-1).1 + fib(x-2).1) }
    }
    use std::thread;
    use std::sync::mpsc::channel;
    let (tx, rx) = channel(); // tx: "transmit", rx: "receive"
    let al = "al";
    thread::spawn(move || {
        tx.send(fib(10));
        println!("you can call me {}", al);
    });
    let f_15 = fib(15).1;
    println!("why am i short of attention");
    let f_10 = rx.recv().unwrap().1; // (this blocks to await data)
    assert_eq!((f_10, f_15), (89, 987));
}
```

#### channels are abstraction, data-race free {.fragment}

## No data races: What about our precious mutation?

## No data races 1: "direct" assign { data-transition="fade-out" }

``` {.rust}
#[test] fn demo_catch_direct() {
    fn fib(x: i64) -> (i64, i64) { // returns `(x, fib(x))`
        if x <= 1 { (x,1) } else { (x, fib(x-1).1 + fib(x-2).1) }
    }
    use std::thread;
    let al = "al";
    let mut f_10_recv = (0, 0);

    thread::spawn(move || {
        f_10_recv = fib(10);
        println!("you can call me {}", al);
    });
    let f_15 = fib(15).1;
    while f_10_recv.0 == 0 { }  // <-- many alarm bells
    let f_10 = f_10_recv.1;
    println!("why am i short of attention");
    assert_eq!((f_10, f_15), (89, 987));
}
```

#### compiles; does not work (no actual communication; implicit copying) {.fragment }

## No data races 2: mut-ref  { data-transition="fade-in" }

``` {.rust .compile_error}
#[test] fn demo_catch_mutref() {
    fn fib(x: i64) -> (i64, i64) { // returns `(x, fib(x))`
        if x <= 1 { (x,1) } else { (x, fib(x-1).1 + fib(x-2).1) }
    }
    use std::thread;
    let al = "al";
    let mut f_10_recv = (0, 0);
    let ptr_recv = &mut f_10_recv; // <-- Okay, say what we meant
    thread::spawn(move || {
        *ptr_recv = fib(10);
        println!("you can call me {}", al);
    });
    let f_15 = fib(15).1;
    while f_10_recv.0 == 0 { }  // <-- many alarm bells
    let f_10 = f_10_recv.1;
    println!("why am i short of attention");
    assert_eq!((f_10, f_15), (89, 987));
}
```

#### does not compile: `spawn` can't share ref to stack-local {.fragment}

# Concurrency as a library concern {.center}

## Libraries can provide new APIs { .center }

### New system interfaces

### New abstractions

# Concurrency: New system interfaces {.center}

## New system interfaces { data-transition="fade-out" }

e.g., Apple's [Grand Central Dispatch][Apple GCD]

[Rust wrapper developed by 3rd party contributor][GCD]

<div class="screen_grab">
![dispatch (from `crates.io`)][dispatch grab]
</div>


[dispatch grab]: curry-on2015/dispatch-cratesio-grab.png
[GCD]: https://crates.io/crates/dispatch
[Apple GCD]: https://developer.apple.com/library/mac/documentation/Performance/Reference/GCD_libdispatch_Ref/index.html

## New system interfaces { data-transition="fade-in" }

`Cargo.toml`{.filename}
```
[dependencies]
dispatch = "0.0.1"
```

```rust
fn demo_gcd() {
    use dispatch::{Queue, QueuePriority};
    let queue = Queue::global(QueuePriority::Default);
    let mut nums = vec![1, 2];
    queue.apply(&mut nums, |x| *x += 1);
    assert!(nums == [2, 3]);
    let nums = queue.map(nums, |x| x.to_string());
    assert!(nums[0] == "2");
}
```

. . .

Implementation almost certainly relies on `unsafe`{.rust} code

"buyer beware" (i.e.  audit, in some manner)

<!--

(Lets test that too.)

```rust
#[test]
fn test_demo_gcd() {
    demo_gcd();
}
```
-->

----

## Demo: `dispatch` misuse, caught! {.center}

``` {.rust .compile_error}
fn demo_gcd2() {
    use dispatch::{Queue, QueuePriority};
    let queue = Queue::global(QueuePriority::Default);
    let mut indices = vec![1, 0];
    let mut nums = vec![1, 2];
    queue.apply(&mut indices, |i| nums[*i] += 1);
    assert!(nums == [2, 3]);
    let nums = queue.map(nums, |x| x.to_string());
    assert!(nums[0] == "2");
}
```

(Type system catches above attempt to close over `&mut`-references in a
closure that is not allowed carry such.)

# Concurrency: New abstractions { .center }

## New abstractions { .center .left_align }

. . .

<h3>Old: `thread::spawn`</h3>

 * child and parent proceed independently

     * (parent can wait for the child, but not required to)

. . .

<p></p>
<h3>New: Fork-join parallelism</h3>

 * parent can share stack-local state with child thread(s)

     * parent *must* wait for child
       (since it shared local state on stack frame)

## New abstractions: Constraints { .center .left_align }

 * `thread::spawn`: child and parent cannot share stack state

 * fork-join: parent must wait for children before returning

We can encode both constraints in Rust

 * Ensures clients obey the relevant protocol!

# Demo of (unstable, unsound) fork join API {.center}

## `thread::scoped`

```rust
fn seq_max(partial_data: &[u8]) -> u8 {
    *partial_data.iter().max().unwrap()
}

fn par_max(data: &[u8]) -> u8 {
    if data.len() <= 4 { return seq_max(data); }
    let len_4 = data.len() / 4; // DATA = [A..B..C..D..]
    let (q1, rest) = data.split_at(len_4); // (A.. \ B..C..D..)
    let (q2, rest) = rest.split_at(len_4); //  (B.. \ C..D..)
    let (q3, q4)   = rest.split_at(len_4); //   (C.. \ D..)
    let t1 = ::std::thread::scoped(|| seq_max(q1)); // fork A..
    let t2 = ::std::thread::scoped(|| seq_max(q2)); // fork B..
    let t3 = ::std::thread::scoped(|| seq_max(q3)); // fork C..
    let v4 = seq_max(q4); //                        compute D..
    let (v1, v2, v3) = (t1.join(), t2.join(), t3.join()); // join!
    return seq_max(&[v1, v2, v3, v4]);
}
```

## `thread::scoped` shows a new trick

  * `thread::spawn` disallowed passing refs to stack-local data

  * Allowing that is the whole point of `thread::scoped`

    * (caveat: `thread::scoped` API is unstable, and undergoing revision due
      to subtle soundness issue)

## Benchmarking `par_max` 1

```rust
extern crate test; use std::iter;
const LIL: usize = 20 * 1024;
const BIG: usize = LIL * 1024;

fn make_data(count: usize) -> Vec<u8> {
    let mut data: Vec<u8> = iter::repeat(10).take(count).collect();
    data.push(200); data.push(3); return data;
}

#[bench] fn bench_big_seq(b: &mut test::Bencher) {
    let data = make_data(BIG);
    b.iter(|| assert_eq!(seq_max(&data), 200));
}
#[bench] fn bench_big_par(b: &mut test::Bencher) {
    let data = make_data(BIG);
    b.iter(|| assert_eq!(par_max(&data), 200));
}
```

```
bench_big_par ... bench:   3,763,711 ns/iter (+/- 1,140,321)
bench_big_seq ... bench:  21,633,799 ns/iter (+/- 2,522,262)
```

## Benchmarking `par_max` 2

```{.rust}
const LIL: usize = 20 * 1024;
const BIG: usize = LIL * 1024;
```

```
bench_big_par ... bench:   3,763,711 ns/iter (+/- 1,140,321)
bench_big_seq ... bench:  21,633,799 ns/iter (+/- 2,522,262)
```

```rust
#[bench] fn bench_lil_seq(b: &mut test::Bencher) {
    let data = make_data(LIL);
    b.iter(|| assert_eq!(seq_max(&data), 200));
}
#[bench] fn bench_lil_par(b: &mut test::Bencher) {
    let data = make_data(LIL);
    b.iter(|| assert_eq!(par_max(&data), 200));
}
```

```
bench_lil_par ... bench:      59,274 ns/iter (+/- 7,756)
bench_lil_seq ... bench:      15,432 ns/iter (+/- 1,961)
```

(`fn par_max` could tune threshold for seq. path)

## What was that about preventing data races?

## `Send`, `Sync`

  * If `T: Send`, then passing (e.g. moving) a `T` to another thread is safe.

  * If `T: Sync`, then copying a `&T` to another thread is safe.

  * (For Rust, "safe" includes "no data races exposed.")

<!-- FIXME: elaborate, add e.g. counter-examples
Or maybe just drop this slide entirely.
-->

# Concurrency: How? {.center}

## Crucial trick: `Send` and `Sync` traits {.center}

If `S: Send`, can transfer `S` across thread boundaries

OTOH: A type `T` implementing `Sync` or might might not be sendable...

. . .

...but `&T` is *always* `Send` for a `T: Sync` (!)

(This is what makes `Sync` interesting)

. . .

Rust compiler *automatically* marks types as `Send` or `Sync`,
based on a recursive analysis of their structure

## Examples {.center .left_align }

Rust compiler *automatically* marks types as `Send` or `Sync`,
based on a recursive analysis of their structure

. . .

e.g.:

 * `i32` is `Send`.

 * `Vec<T>`: `Send` iff `T: Send`

. . .

 * `Box<T>` (owned): `Send` iff `T: Send`, `Sync` iff `T: Sync`

    * Why `Send` rule? because can move `T` out of a (consumed) box.

. . .

 * `Rc<T>` (shared): neither `Send` nor `Sync` (for any `T`)

 * `Arc<T>` (shared): always requires `T: Send + Sync`
     <!-- * (aside: why require `Send`? I have theories...) -->

. . .

(remember that `Rc<T>` and `Arc<T>` do not allow `&mut` access
to `T` itself)

## `Send`/`Sync` demo: `Box`/`Rc`/`Arc`

```rust
fn is_sync<T:Sync>(t: T) {} // Felix: too lazy to construct
fn is_send<T:Send>(t: T) {} //   appropriate channels

#[test]
fn demo_send_sync_vals_refs_box_rc_arc() {
    use std::rc::Rc;
    use std::sync::Arc;
    is_sync(3);
    is_sync(vec![1,2,3]);
    is_send(3);
    is_send(&vec![1,2,3]);

    is_sync(&3);
    is_sync(&vec![1,2,3]);
    is_send(&3);
    is_send(&vec![1,2,3]);

    is_sync(Box::new(3));
    is_sync(Box::new(vec![1,2,3]));
    is_send(Box::new(3));
    is_send(Box::new(vec![1,2,3]));
}
```

## `Send`/`Sync` demo: `Box` Rules {.center}

```rust
#[test]
fn demo_send_sync_box_rules() {
    fn take_send<T:Send+Clone>(t: T) {
        // is_sync(Box::new(t.clone())); // (STATIC ERROR)
        is_send(Box::new(t.clone()));
        // is_sync(&Box::new(t.clone())); // (STATIC ERROR)
        // is_send(&Box::new(t.clone())); // (STATIC ERROR)
    }
    fn take_sync<T:Sync+Clone>(t: T) {
        is_sync(Box::new(t.clone()));
        // is_send(Box::new(t.clone())); // (STATIC ERROR)
        is_sync(&Box::new(t.clone()));
        is_send(&Box::new(t.clone()));
    }
    fn take_send_sync<T:Send+Sync+Clone>(t: T) {
        is_sync(Box::new(t.clone()));
        is_send(Box::new(t.clone()));
        is_sync(&Box::new(t.clone()));
        is_send(&Box::new(t.clone()));
    }
}
```

## `Send`/`Sync` demo: `Rc` values {.center}

```rust
#[test]
fn demo_send_sync_rc() {
    use std::rc::Rc;

    // is_sync(Rc::new(3)); // (STATIC ERROR)
    // is_sync(Rc::new(vec![1,2,3])); // (STATIC ERROR)
    // is_send(Rc::new(3)); // (STATIC ERROR)
    // is_send(Rc::new(vec![1,2,3])); // (STATIC ERROR)

    // is_sync(vec![Rc::new(1)]); // (STATIC ERROR)
    // is_send(vec![Rc::new(1)]); // (STATIC ERROR)
    // is_sync(&vec![Rc::new(1)]); // (STATIC ERROR)
    // is_send(&vec![Rc::new(1)]); // (STATIC ERROR)

    // is_sync(Box::new(Rc::new(1))); // (STATIC ERROR)
    // is_send(Box::new(Rc::new(1))); // (STATIC ERROR)
    // is_sync(&Box::new(Rc::new(1))); // (STATIC ERROR)
    // is_send(&Box::new(Rc::new(1))); // (STATIC ERROR)
}
```

## `Send`/`Sync` demo: `Arc` values {.center}

```rust
fn demo_send_sync_arc() {
    use std::sync::Arc;
    is_sync(vec![Arc::new(1)]);
    is_send(vec![Arc::new(1)]);
    is_sync(&vec![Arc::new(1)]);
    is_send(&vec![Arc::new(1)]);

    is_sync(Arc::new(3));
    is_sync(Arc::new(vec![1,2,3]));
    is_send(Arc::new(3));
    is_send(Arc::new(vec![1,2,3]));
}
```

## `Send`/`Sync` demo: `Arc` rules {.center}

```rust
#[test]
fn demo_send_sync_arc_rules() {
    use std::sync::Arc;
    fn take_send<T:Send+Clone>(t: T) {
        // is_sync(Arc::new(t.clone())); // (STATIC ERROR)
        // is_send(Arc::new(t.clone())); // (STATIC ERROR)
        // is_sync(&Arc::new(t.clone())); // (STATIC ERROR)
        // is_send(&Arc::new(t.clone())); // (STATIC ERROR)
    }
    fn take_sync<T:Sync+Clone>(t: T) {
        // is_sync(Arc::new(t.clone())); // (STATIC ERROR)
        // is_send(Arc::new(t.clone())); // (STATIC ERROR)
        // is_sync(&Arc::new(t.clone())); // (STATIC ERROR)
        // is_send(&Arc::new(t.clone())); // (STATIC ERROR)
    }
    fn take_send_sync<T:Send+Sync+Clone>(t: T) {
        is_sync(Arc::new(t.clone()));
        is_send(Arc::new(t.clone()));
        is_sync(&Arc::new(t.clone()));
        is_send(&Arc::new(t.clone()));
    }
}
```

## `Send`/`Sync` demo: `Cell` and `RefCell` values {.center}

```rust
#[test]
fn demo_send_sync_cell_refcell() {
    use std::cell::Cell;
    use std::cell::RefCell;

    // is_sync(Cell::new(3)); // (STATIC ERROR)
    // Cell::new(vec![1,2,3]);
    is_send(Cell::new(3));

    // is_sync(&Cell::new(3)); // (STATIC ERROR)
    // is_send(&Cell::new(3)); // (STATIC ERROR)

    // is_sync(RefCell::new(3)); // (STATIC ERROR)
    // is_sync(RefCell::new(vec![1,2,3])); // (STATIC ERROR)
    is_send(RefCell::new(3));
    is_send(RefCell::new(vec![1,2,3]));
}
```
