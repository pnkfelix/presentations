# Pointers, Smart and Otherwise {.center}

## (More pictures) {.center}

## Stack allocation

``` {.rust}
let b = B::new();
```

![stack allocation](stackB.png)

----

``` {.rust}
let b = B::new();

let r1: &B = &b;
let r2: &B = &b;
```

![stack allocation and immutable borrows](imm_borrows_stackB.png)

(`b` has lost write capability)

----

``` {.rust}
let mut b = B::new();

let w: &mut B = &mut b;
```

![stack allocation and mutable borrows](mutable_borrow_stackB.png)

(`b` has temporarily lost both read *and* write capabilities)

## Heap allocation: `Box<B>`

``` {.rust}
let a = Box::new(B::new());
```

![pristine boxed B](box_baseline_rw0.png)

`a` (as owner) has both read and write capabilities

## Immutably borrowing a box

``` {.rust}
let a = Box::new(B::new());
let r_of_box: &Box<B> = &a; // (not directly a ref of B)

let r1: &B = &*a;
let r2: &B = &a; // <-- coercion!
```

![immutable borrows of heap-allocated B](box_imm_borrows.png)

`a` retains read capabilities (has temporarily lost write)

## Mutably borrowing a box

``` {.rust}
let mut a = Box::new(B::new());

let w: &mut B = &mut a; // (again, coercion happening here)
```

![mutable borrow of heap-allocated B](box_mutable_borrow.png)

`a` has temporarily lost *both* read and write capabilities

## Heap allocation: `Vec<B>`

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
```

![vec, filled to capacity](vec_push_realloc_0pre.png)

## Vec Reallocation

``` {.rust}
...
a.push(B::new());
```

----------------------------------------------------- --- -----------------------------------------------------
before                                                    after
![vec, filled to capacity](vec_push_realloc_0pre.png)     ![vec, reallocated](vec_push_realloc_1post.png)
----------------------------------------------------- --- -----------------------------------------------------


## Slices: borrowing *parts* of an array {.center}

## Basic `Vec<B>`

![pristine unborrowed vec](vec_baseline_rw0.png)

(`a` has read and write capabilities)

## Immutable borrowed slices

![mutiple borrowed slices vec](vec_slices.png)

(`a` has only read capability now; shares it with `r1` and `r2`)

## Immutable borrows can safely overlap

![overlapping slices](vec_slices_overlapping.png)

## Basic `Vec<B>` again

![pristine unborrowed vec](vec_baseline_rw0.png)

(`a` has read and write capabilities)

## Mutable slice of whole vec

![mutable slice of vec](vec_slice_mut.png)

(`a` has *no* capabilities; `w` now has read and write capability)

## Mutable disjoint slices

![disjoint mutable borrows](vec_split_mut_at.png)

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

## Dynamic Exclusivity

``` {.rust}
let b = Box::new(RefCell::new(B::new()));

let r1: &RefCell<B> = &b;
let r2: &RefCell<B> = &b;
```

![box of refcell](box_refcell_borrows.png)

----

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

----

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone(); // increments ref-count on heap-alloc'd value
```

![shared ownership of refcell](rc_refcell_baseline.png)

----

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone();
let r1: &RefCell<B> = &rc1;
let r2: &RefCell<B> = &rc2; // (or even just `r1`)
```

![borrows of refcell can alias](rc_refcell_readers.png)

----

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone();
let w = rc2.borrow_mut();
```

![there can be only one!](rc_refcell_w.png)

## Other kinds of shared ownership

 * `TypedArena<T>`

 * `Cow<T>`

 * `Rc<T>` vs `Arc<T>`
