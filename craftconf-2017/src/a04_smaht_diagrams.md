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

(`a` has read and write capabilities)

## Immutable borrowed slices: `&[T]`

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

(`a` has read and write capabilities)

## Mutable slice: `&mut [T]`

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
|   rc1 --------=-+->|   2    |
|             |   |  +--------+
|   rc2 --------=-'  |   B    |
|             |      |        |
|             |      |        |
|             |      '--------'
|             |
     . . .
|             |
+-------------+
```

(`rc1` and `rc2` each have read access; but neither can
statically assume exclusive (`mut`) access, nor can they
provide `&mut` borrows without assistance.)
