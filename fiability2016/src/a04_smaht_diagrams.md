# Pointers, Smart and Otherwise {.center}

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

![immutable borrows of heap-allocated B; `a` retains read capabilities (has temporarily lost write)](memory-pics-svg/stack-a-owns-heap-b-borrows-r1-and-r2.svg)



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
