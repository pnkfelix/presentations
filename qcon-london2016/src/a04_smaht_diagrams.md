# Pointers, Smart and Otherwise {.center}

## (More pictures) {.center}

## Stack allocation

``` {.rust}
let b = B::new();
```

![stack allocation](memory-pics-svg/stack-allocation.svg)

----

``` {.rust}
let b = B::new();

let r1: &B = &b;
let r2: &B = &b;
```

![stack allocation and immutable borrows](memory-pics-svg/stack-b-with-r1-and-r2.svg)

(`b` has lost write capability)

----

``` {.rust}
let mut b = B::new();

let w: &mut B = &mut b;
```

![stack allocation and mutable borrows](memory-pics-svg/stack-b-with-w.svg)

(`b` has temporarily lost both read *and* write capabilities)

## Heap allocation: `Box<B>`

``` {.rust}
let a = Box::new(B::new());
```

![pristine boxed B](memory-pics-svg/stack-a-owns-heap-b.svg)

`a` (as owner) has both read and write capabilities

## Immutably borrowing a box

``` {.rust}
let a = Box::new(B::new());
let r_of_box: &Box<B> = &a; // (not directly a ref of B)

let r1: &B = &*a;
let r2: &B = &a; // <-- coercion!
```

![immutable borrows of heap-allocated B](memory-pics-svg/stack-a-owns-heap-b-borrows-r1-and-r2.svg)

`a` retains read capabilities (has temporarily lost write)

## Mutably borrowing a box

``` {.rust}
let mut a = Box::new(B::new());

let w: &mut B = &mut a; // (again, coercion happening here)
```

![mutable borrow of heap-allocated B](memory-pics-svg/stack-a-owns-heap-b-mut-borrow-w.svg)

`a` has temporarily lost *both* read and write capabilities

## Heap allocation: `Vec<B>`

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
```

![vec, filled to capacity](vector-and-slice-pics-svg/vec-reallocation-before.svg)

## Vec Reallocation

``` {.rust}
...
a.push(B::new());
```

--------------------------------------------------------------------------------- --- -------------------------------------------------------------------------
before                                                                                after
![vec, filled to capacity](vector-and-slice-pics-svg/vec-reallocation-before.svg)     ![vec, reallocated](vector-and-slice-pics-svg/vec-reallocation-after.svg)
--------------------------------------------------------------------------------- --- -------------------------------------------------------------------------


## Slices: borrowing *parts* of an array {.center}

## Basic `Vec<B>`

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
```

![pristine unborrowed vec](vector-and-slice-pics-svg/pristine-vec.svg)

(`a` has read and write capabilities)

## Immutable borrowed slices

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
let r1 = &a[0..3];
let r2 = &a[7..n-4];
```

![mutiple borrowed slices vec](vector-and-slice-pics-svg/vec-with-two-disjoint-imm-slices.svg)

(`a` has only read capability now; shares it with `r1` and `r2`)

## Safe overlap between `&[..]`

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
let r1 = &a[0..7];
let r2 = &a[3..n-4];
```

![overlapping slices](vector-and-slice-pics-svg/vec-with-two-overlapping-imm-slices.svg)

## Basic `Vec<B>` again

![pristine unborrowed vec](vector-and-slice-pics-svg/pristine-vec.svg)

(`a` has read and write capabilities)

## Mutable slice of whole vec

```rust
let w = &mut a[0..n];
```

![mutable slice of vec](vector-and-slice-pics-svg/vec-with-whole-mut-slice.svg)

(`a` has *no* capabilities; `w` now has read and write capability)

## Mutable disjoint slices

```rust
let (w1,w2) = a.split_at_mut(n-4);
```

![disjoint mutable borrows](vector-and-slice-pics-svg/vec-with-two-disjoint-mut-slices.svg)

(`w1` and `w2` share read and write capabilities for disjoint portions)

## Shared *Ownership* {.center}

## Shared Ownership

``` {.rust}
let rc1 = Rc::new(B::new());
let rc2 = rc1.clone(); // increments ref-count on heap-alloc'd value
```

![shared ownership via ref counting](rc_baseline.png)

(`rc1` and `rc2` each have read access; but neither can
statically assume exclusive (`mut`) access, nor can they
provide `&mut` borrows without assistance.)

## Dynamic Exclusivity {.center}

## `RefCell<T>`: Dynamic Exclusivity

``` {.rust}
let b = Box::new(RefCell::new(B::new()));

let r1: &RefCell<B> = &b;
let r2: &RefCell<B> = &b;
```

![box of refcell](box_refcell_borrows.png)

## `RefCell<T>`: Dynamic Exclusivity

``` {.rust}
let b = Box::new(RefCell::new(B::new()));
let r1: &RefCell<B> = &b;
let r2: &RefCell<B> = &b;
let w = r2.borrow_mut(); // if successful, `w` acts like `&mut B`
```

![fallible mutable borrow](box_refcell_writer.png)

``` {.rust}
// below panics if `w` still in scope
let w2 = b.borrow_mut();
```

## Previous generalizes to shared ownership {.big_text .center}

## `Rc<RefCell<T>>`

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone(); // increments ref-count on heap-alloc'd value
```

![shared ownership of refcell](rc_refcell_baseline.png)

## `Rc<RefCell<T>>`

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone();
let r1: &RefCell<B> = &rc1;
let r2: &RefCell<B> = &rc2; // (or even just `r1`)
```

![borrows of refcell can alias](rc_refcell_readers.png)

## `Rc<RefCell<T>>`

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone();
let w = rc2.borrow_mut();
```

![there can be only one!](rc_refcell_w.png)

## What static guarantees does `Rc<RefCell<T>>` have?

. . .

Not much!

. . .

If you want to port an existing *imperative* algorithm with all sorts of
sharing, you *could* try using `Rc<RefCell<T>>`.

You then might spend much less time wrestling with Rust's type (+borrow) checker.

. . .

The point: `Rc<RefCell<T>>` is nearly an anti-pattern. It limits static reasoning. You should avoid it if you can.

## Other kinds of shared ownership

 * `TypedArena<T>`

 * `Cow<T>`

 * `Rc<T>` vs `Arc<T>`
