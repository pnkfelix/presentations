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

## {.center}

----------------- -------- -------------
Ownership         `T`
Exclusive access  `&mut T` ("mutable")
Shared access     `&T`     ("read-only")
----------------- -------- -------------

# A Metaphor {.center}

## (reminder: metaphors never work 100%) {.big_text .center}

----

``` {.rust}
let christine = Car::new();
```

This is "Christine"

![pristine unborrowed car](christine_car_pristine.png)

(apologies to Stephen King)

----

``` {.rust}
let read_only_borrow = &christine;
```

![single inspector (immutable borrow)](christine_car_single_inspector.png)

(apologies to Randall Munroe))

----

``` {.rust}
read_only_borrows[2] = &christine;
read_only_borrows[3] = &christine;
read_only_borrows[4] = &christine;
```

![many inspectors (immutable borrows)](christine_car_many_inspectors.png)

----

When inspectors are finished, we are left again with:

![pristine unborrowed car](christine_car_pristine.png)

----

``` {.rust}
let mutable_borrow = &mut christine;
give_arnie(mutable_borrow);
```

![driven car (mutably borrowed)](christine_car_driven.png)

## Can't mix the two in safe code! {.center}

### Otherwise: (data) races!

----

``` {.rust .compile_error}
read_only_borrows[2] = &christine;
let mutable_borrow = &mut christine;
read_only_borrows[3] = &christine;
// ⇒ CHAOS!
```

![mixing mutable and immutable is illegal](christine_car_driving_over_inspectors.png)

# Exclusive access

## `&mut`: can I borrow the car?

<div class="notes">
If I own a car, and I lend the keys to Arnie, I *still* own the car
</div>

<!--
```rust
fn invite_friend_over() -> Arnie {
    Arnie { fav_color: Color::Black, partner: NoGirlfriend }
}
type Location = ();
static work: Location = ();
impl Car {
    fn new() -> Car { Car { color: Color::Red, engine: Engine::VintageV8 } }
    fn drive_to(&mut self, l: Location) { }
}
```
-->

```rust
fn borrow_the_car_1() {
    let mut christine = Car::new();
    {
        let car_keys = &mut christine;
        let arnie = invite_friend_over();
        arnie.lend(car_keys);
    } // end of scope for `arnie` and `car_keys`
    christine.drive_to(work); // I still own the car!
}
```

. . .

But when her keys are elsewhere, I cannot drive `christine`!

``` {.rust .compile_error}
fn borrow_the_car_2() {
    let mut christine = Car::new();
    {
        let car_keys = &mut christine;
        let arnie = invite_friend_over();
        arnie.lend(car_keys);
        christine.drive_to(work); // <-- compile error
    } // end of scope for `arnie` and `car_keys`
}
```

## Extending the metaphor

<!--
```rust
use std::rc::Rc;
use std::cell::RefCell;
struct Arnie<GF=NoGirlfriend> { fav_color: Color, partner: GF }
struct ArnieLongTermRelationship<'a> { partner: &'a Leigh<'a> }
struct NoGirlfriend;
struct Leigh<'a> { car: RefCell<Option<&'a mut Car>> }
trait Partner<'a> {
    fn lend(&self, c: &mut Car) { }
    fn take(&'a self, c: &'a mut Car) { }
}
impl<'a> Partner<'a> for NoGirlfriend { }
impl<'a> Partner<'a> for Leigh<'a> {
    fn take(&'a self, c: &'a mut Car) {
        *self.car.borrow_mut() = Some(c);
    }
}
impl Arnie { fn lend(&self, c: &mut Car) { lend_1(self, c); lend_2(self, c); } }
```
-->

Possessing the keys, Arnie could take the car for a new paint job.

```rust
fn lend_1(arnie: &Arnie, k: &mut Car) { k.color = arnie.fav_color; }
```

Or lend keys to someone else (*reborrowing*) before paint job

```rust
fn lend_2(arnie: &Arnie, k: &mut Car) {
    arnie.partner.lend(k); k.color = arnie.fav_color;
}
```


## End of metaphor

### (on to models)
