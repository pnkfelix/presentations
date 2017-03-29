# Pointers, Smart and Otherwise {.center}

## (More pictures) {.center}

## Stack allocation

``` {.rust}
let b = B::new();
```

```art
+-------------+
|             |
|   +------+  |
|   |  B   |  |
|   |      |  |
|   |      |  |
|   +------+  |
|             |
|             |
|             |
|             |
|             |
     . . .
|             |
+-------------+
```
stack allocation

![stack allocation](stackB.png)

----

``` {.rust}
let b = B::new();

let r1: &B = &b;
let r2: &B = &b;
```

```art
+-------------+
| (!w)        |
|   +------+ r
|   |  B   |<------.
|   |      |<----. |
|   |      | r|  | |
|   +------+  |  : |
|             |  | |
|     r1 --------' |
|             |    :
|             |    |
|     r2 ----------'
|             |
|             |
     . . .
|             |
+-------------+

```

stack allocation and immutable borrows
![stack allocation and immutable borrows](imm_borrows_stackB.png)

(`b` has lost write capability)

----

``` {.rust}
let mut b = B::new();

let w: &mut B = &mut b;
```


```art
+-------------+
| (!rw)       |
|   +------+ rw 
|   |  B   |<----.
|   |      |  |  :
|   |      |  |  :
|   +------+  |  :
|             |  :
|     w  --------'
|             |
|             |
|             |
     . . .
|             |
+-------------+
```

stack allocation and mutable borrows

<!-- ![stack allocation and mutable borrows](mutable_borrow_stackB.png) -->

(`b` has temporarily lost both read *and* write capabilities)

## Heap allocation: `Box<B>`

``` {.rust}
let a = Box::new(B::new());
```

```art
+-------------+
|             |   rw .--------.
|   a -------------->|   B    |
|             |      |        |
|             |      |        |
|             |      '--------'
|             |
|             |
|             |
|             |
     . . .
|             |
+-------------+
```

pristine boxed `B`

<!-- ![pristine boxed B](box_baseline_rw0.png) -->

`a` (as owner) has both read and write capabilities

## Immutably borrowing a box

``` {.rust}
let a = Box::new(B::new());
let r_of_box: &Box<B> = &a; // (not directly a ref of B)

let r1: &B = &*a;
let r2: &B = &a; // <-- coercion!
```

```art
+-------------+
|             |   r  .--------.
|   a -------------->|   B    |
|             |    r |        |
|             | .--->|        |
|             | : .->'--------'
|    r1 --------' :r 
|             |   :
|    r2 ----------'
|             |
|             |
     . . .
|             |
+-------------+
```

immutable borrows of heap-allocated `B`

<!-- ![immutable borrows of heap-allocated B](box_imm_borrows.png) -->

`a` retains read capabilities (has temporarily lost write)

## Mutably borrowing a box

``` {.rust}
let mut a = Box::new(B::new());

let w: &mut B = &mut a; // (again, coercion happening here)
```

```art
+-------------+
|             |      .--------.
|   a -------------->|   B    |
|             |      |        |
|             | .--->|        |
|             | : rw '--------'
|    w  --------'
|             |
|             |
|             |
|             |
     . . .
|             |
+-------------+
```

![mutable borrow of heap-allocated B](box_mutable_borrow.png)

`a` has temporarily lost *both* read and write capabilities

## Heap allocation: `Vec<B>`

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
```

```art
   a ---------.
              |rw
              v
           .-------------.
           |    B_1      |
           |             |
           |    B_2      |
           |             |
           |    B_3      |
           |             |
           |   . . .     |
           |    B_n      |
           '-------------'
```

![vec, filled to capacity](vec_push_realloc_0pre.png)

## Vec Reallocation

``` {.rust}
...
a.push(B::new());
```

```art
+-----------------------------------------+   +------------------------------------------------+
| before                                  |   |  after                                         |
|                                         |   |                                                |
|   a ---------.                          |   |     a ---------------------------.             |
|              |rw                        |   |                                  |rw           |
|              v                          |   |                                  v             |
|           .-------------.               |   |            .-------------.    .-------------.  |
|           |    B_1      |               |   |            |    B_1      | => |    B_1      |  |
|           |             |               |   |            |             |    |             |  |
|           |    B_2      |               |   |            |    B_2      | => |    B_2      |  |
|           |             |               |   |            |             |    |             |  |
|           |    B_3      |               |   |            |    B_3      | => |    B_3      |  |
|           |             |               |   |            |             |    |             |  |
|           |   . . .     |               |   |            |   . . .     |    |   . . .     |  |
|           |    B_n      |               |   |            |    B_n      | => |    B_n      |  |
|           '-------------'               |   |            '-------------'    |             |  |
|                                         |   |                               |    B_n+1    |  |
|                                         |   |                               |             |  |
|                                         |   |                               |             |  |
|                                         |   |                               |             |  |
|                                         |   |                               |             |  |
|                                         |   |                               |   . . .     |  |
|                                         |   |                               |             |  |
|                                         |   |                               '-------------'  |
+-----------------------------------------+   +------------------------------------------------+
```

----------------------------------------------------- --- -----------------------------------------------------
before                                                    after
![vec, filled to capacity](vec_push_realloc_0pre.png)     ![vec, reallocated](vec_push_realloc_1post.png)
----------------------------------------------------- --- -----------------------------------------------------


## Slices: borrowing *parts* of an array {.center}

## Basic `Vec<B>`

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
```

```art
   a ---------------.
                    | rw
                    v
                   .-------------.
                   |    B_1      |
                   |             |
                   |    B_2      |
                   |             |
                   |    B_3      |
                   |             |
                   |             |
                   |   . . .     |
                   |             |
                   |             |
                   |    B_n      |
                   '-------------'
```

![pristine unborrowed vec](vec_baseline_rw0.png)

(`a` has read and write capabilities)

## Immutable borrowed slices

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
let r1 = &a[0..3];
let r2 = &a[7..n-4];
```

```art
   a -------------.
                  | r
                  v
                 .-------------.
            r ┬  |    B_1      |
   r1 -=----->|  |             |
              |  |    B_2      |
              |  |             |
              |  |    B_3      |
              ┴  |             |
                 |             |
            r ┬  |   . . .     |
   r2 -=----->|  |             |
              ┴  |             |
                 |    B_n      |
                 '-------------'
```

![mutiple borrowed slices vec](vec_slices.png)

(`a` has only read capability now; shares it with `r1` and `r2`)

## Safe overlap between `&[..]`

``` {.rust}
let mut a = Vec::new();
for i in 0..n { a.push(B::new()); }
let r1 = &a[0..7];
let r2 = &a[3..n-4];
```

```art
   a -------------.
                  | r
                  v
                 .-------------.
         r ┬     |    B_1      |
   r1 -=-->|     |             |
           |     |    B_2      |
           |     |             |
           |   ┬ |    B_3      |
           |   | |             |
           ┴   | |             |
             r | |   . . .     |
   r2 -=------>| |             |
               ┴ |             |
                 |    B_n      |
                 '-------------'
```


![overlapping slices](vec_slices_overlapping.png)

## Basic `Vec<B>` again

```art
   a ---------------.
                    | rw
                    v
                   .-------------.
                   |    B_1      |
                   |             |
                   |    B_2      |
                   |             |
                   |    B_3      |
                   |             |
                   |             |
                   |   . . .     |
                   |             |
                   |             |
                   |    B_n      |
                   '-------------'
```

![pristine unborrowed vec](vec_baseline_rw0.png)

(`a` has read and write capabilities)

## Mutable slice of whole vec

```rust
let w = &mut a[0..n];
```

```art
   a ---------------.
                    |
                    v
         rw ┬      .-------------.
   w -=---->|      |    B_1      |
            |      |             |
            |      |    B_2      |
            |      |             |
            |      |    B_3      |
            |      |             |
            |      |             |
            |      |   . . .     |
            |      |             |
            |      |             |
            |      |    B_n      |
            ┴      '-------------'
```

![mutable slice of vec](vec_slice_mut.png)

(`a` has *no* capabilities; `w` now has read and write capability)

## Mutable disjoint slices

```rust
let (w1,w2) = a.split_at_mut(n-4);
```

```art
   a ---------------.
                    |
                    v
         rw ┬      .-------------.
   w1 -=--->|      |    B_1      |
            |      |             |
            |      |    B_2      |
            |      |             |
            |      |    B_3      |
            |      |             |
            |      |             |
            ┴      |             |
         rw ┬      |             |
   w2 -=--->|      |             |
            |      |    B_n      |
            ┴      '-------------'
```

![disjoint mutable borrows](vec_split_mut_at.png)

(`w1` and `w2` share read and write capabilities for disjoint portions)

## Shared *Ownership* {.center}

## Shared Ownership

``` {.rust}
let rc1 = Rc::new(B::new());
let rc2 = rc1.clone(); // increments ref-count on heap-alloc'd value
```

```art
+-------------+
|             |    r .--------.
|   rc1 --------=--->|   B    |
|             |      |        |
|             | .--->|        |
|             | :  r '--------'
|   rc2 --------'
|             |
|             |
|             |
|             |
     . . .
|             |
+-------------+
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

```art
b: Box<RefCell<B>> ----------.
                              \ (  )
                               '---> .-------.
                                     |       |
                                (  ) |       |
r1  -------------------------------> |   B   |
                                     |       |
                                (  ) |       |
r2  -------------------------------> |       |
                                     |       |
                                     |       |
                                     |       |
                                     '-------'
```

![box of refcell](box_refcell_borrows.png)

## `RefCell<T>`: Dynamic Exclusivity

``` {.rust}
let b = Box::new(RefCell::new(B::new()));
let r1: &RefCell<B> = &b;
let r2: &RefCell<B> = &b;
let w = r2.borrow_mut(); // if successful, `w` acts like `&mut B`
```

```art
                                (  )
b: Box<RefCell<B>> ----------------> .-------.
                                     |       |
                                (  ) |       |
r1  -------------------------------> |   B   |
                                     |       |
                                (  ) |       |
r2  -------------------------------> |       |
                                     |       |
                                 rw  |       |
 w  -------------------------------> |       |
                                     '-------'
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

```art
                                (rw)
rc1: Rc<RefCell<B>> ---------------> .-------.
                                     |       |
                                (rw) |       |
rc2: Rc<RefCell<B>> ---------------> |   B   |
                                     |       |
                                     |       |
                                     |       |
                                     |       |
                                     |       |
                                     '-------'
```

![shared ownership of refcell](rc_refcell_baseline.png)

## `Rc<RefCell<T>>`

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone();
let r1: &RefCell<B> = &rc1;
let r2: &RefCell<B> = &rc2; // (or even just `r1`)
```

```art
                                (r )
rc1: Rc<RefCell<B>> ---------------> .-------.
                                     |       |
                                (r ) |       |
rc2: Rc<RefCell<B>> ---------------> |   B   |
                                     |       |
                                 r   |       |
r1 --------------------------------> |       |
                                 r   |       |
r2 --------------------------------> |       |
                                     '-------'
```

![borrows of refcell can alias](rc_refcell_readers.png)

## `Rc<RefCell<T>>`

``` {.rust}
let rc1 = Rc::new(RefCell::new(B::new()));
let rc2 = rc1.clone();
let w = rc2.borrow_mut();
```

```art
                                (  )
rc1: Rc<RefCell<B>> ---------------> .-------.
                                     |       |
                                (  ) |       |
rc2: Rc<RefCell<B>> ---------------> |   B   |
                                     |       |
                                 rw  |       |
 w --------------------------------> |       |
                                     |       |
                                     |       |
                                     '-------'
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
