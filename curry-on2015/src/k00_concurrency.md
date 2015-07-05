# Concurrency { .no_center }

## Libraries can provide new APIs { .center }

### New abstractions

### New system interfaces

## New abstractions

## New system interfaces { data-transition="fade-out" }

e.g., Apple's [Grand Central Dispatch][Apple GCD]

[Rust wrapper developed by 3rd party contributor][GCD]

<div class="screen_grab">
![dispatch][dispatch grab]
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

(These usually rely on `unsafe`{.rust} code;
one must audit in some manner.)

----

(Lets test that too.)

```rust
#[test]
fn test_demo_gcd() {
    demo_gcd();
}
```

----

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
