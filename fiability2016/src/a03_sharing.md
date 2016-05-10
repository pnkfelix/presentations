# Sharing Data: Ownership and References { .center }

<!--
```rust
#[derive(Copy, Clone)]
enum Color { Red, Black }
enum Engine { BrokenV8, VintageV8 }
struct Apartment;
type ApartmentBuilding = Vec<Apartment>;
type Home = Apartment;
```
-->

## Rust types

Move                     Copy               Copy if `T:Copy`
-----------------------  ------------------ ----------------------------
`Vec<T>`, `String`, ...  `i32`, `char`, ... `[T; n]`, `(T1,T2,T3)`, ...

. . .

```rust
struct Car { color: Color, engine: Engine }

fn demo_ownership() {
    let mut used_car: Car = Car { color: Color::Red,
                                  engine: Engine::BrokenV8 };
    let apartments = ApartmentBuilding::new();
```

. . .

references to data (`&mut T`, `&T`):

```rust
    let my_home: &Home;      // <-- an "immutable" borrow
    let christine: &mut Car; // <-- a "mutable" borrow
    my_home = &apartments[6]; //      (read `mut` as "exclusive")
    let neighbors_home = &apartments[5];
    christine = &mut used_car;
    christine.engine = Engine::VintageV8;
}
```

## Why multiple `&`-reference types? {.center}

. . .

 * Distinguish *exclusive* access from *shared* access

 * Enables **safe**, **parallel** API's

# Borrowing: A Metaphor (continued) {.center}

## (reminder: metaphors never work 100%) {.big_text .center}

----

``` {.rust}
let christine = Car::new();
```

This is "Christine"

![pristine unborrowed car](christine-svg/pristine-car.svg)

(apologies to Stephen King)

----

``` {.rust}
let read_only_borrow = &christine;
```

![single inspector (immutable borrow)](christine-svg/show-one-inspector.svg)

----

``` {.rust}
read_only_borrows[2] = &christine;
read_only_borrows[3] = &christine;
read_only_borrows[4] = &christine;
```

![many inspectors (immutable borrows)](christine-svg/show-many-inspectors.svg)

----

When inspectors are finished, we are left again with:

![pristine unborrowed car](christine-svg/pristine-car.svg)

----

``` {.rust}
let mutable_borrow = &mut christine; // like taking keys ...
give_arnie(mutable_borrow); // ... and giving them to someone
```

![driven car (mutably borrowed)](christine-svg/show-driver-alone.svg)

## Can't mix the two in safe code! {.center}

------------------------------------------------------------------------------ ----------------------------------------------------------
![many inspectors (immutable borrows)](christine-svg/show-many-inspectors.svg) ![driven car (mutably borrowed)](christine-svg/show-driver-alone.svg)
------------------------------------------------------------------------------ ----------------------------------------------------------


### Otherwise: (data) races!

----

``` {.rust .compile_error}
read_only_borrows[2] = &christine;
let mutable_borrow = &mut christine;
read_only_borrows[3] = &christine;
// ⇒ CHAOS!
```

![mixing mutable and immutable is illegal](christine-svg/show-driver-and-corpses.svg)

## {.center}

----------------- -------- -------------
Ownership         `T`
Exclusive access  `&mut T` ("mutable")
Shared access     `&T`     ("read-only")
----------------- -------- -------------

TODO: Vec example (in C++ perhaps?)

. . .

Reminder: this does not apply only to concurrency (iterator invalidation, etc.)

. . .

Now let's see how we can apply that to build safe abstractions
