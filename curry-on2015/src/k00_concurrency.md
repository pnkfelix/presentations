# Concurrency { .center }

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
