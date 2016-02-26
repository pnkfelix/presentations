# Ownership and sharing { .center }

<!--
```rust
enum Color { Red, Black }
enum Engine { BrokenV8, VintageV8 }
struct Apartment;
type ApartmentBuilding = Vec<Apartment>;
type Home = Apartment;
```
-->

## Rust types

Basic data types (`T`): `String`, `Vec<T>`, `i32`

```rust
struct Car { color: Color, engine: Engine }

fn demo_ownership() {
    let mut used_car: Car = Car { color: Color::Red,
                                  engine: Engine::BrokenV8 };
    let apartments = ApartmentBuilding::new();
```

. . .

Primitive references to data (`&T`, `&mut T`):

```rust
    let my_home: &Home;
    let christine: &mut Car;

    my_home = &apartments[6];
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

## `&mut`: can I borrow the car?

<div class="notes">
If I own a car, and I lend the keys to Arnie, I *still* own the car
</div>

<!--
```rust
fn invite_friend_over() -> Arnie { Arnie { partner: NoGirlfriend } }
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

## Todo Code Link

<!--
```rust
use std::rc::Rc;
use std::cell::RefCell;
struct Arnie<GF=NoGirlfriend> { partner: GF }
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
impl Arnie { fn lend(&self, c: &mut Car) { self.lend_1(c); } }
```
-->

Possessing the keys, Arnie can take car for a new paint job.

```rust
impl Arnie { fn lend_1(&self, k: &mut Car) { k.color = Color::Black; } }
```

Or he can lend keys to someone else (*reborrowing*) before paint job

```rust
impl Arnie {
    fn lend_2(&self, k: &mut Car) {
        self.partner.lend(k); k.color = Color::Black;
    }
}
```

(Can even *transfer* exclusive access; *Arnie* never gets keys back!)

``` {.rust .compile_error}
        self.partner.take(k); k.color = Color::Black;
        //                    ~~~~~~~~~~~~~~~~~~~~~~
        // error: cannot assign to `k.color` because it is borrowed
```

<!-- Not sure this snippet pays for itself so commenting out for now

``` {.rust .compile_error}
impl<'b> ArnieLongTermRelationship<'b> {
    fn lend_3(&self, k: &'b mut Car) {
        self.partner.take(k); k.color = Color::Black;
        // error: cannot assign to `k.color` because it is borrowed
    }
}
```
-->

----

Owner loses capabilities attached to `&mut`-borrows only
*temporarily* (*)

<div class="notes">
Keys *will* return after `&mut`-borrow ends; I, the owner, will regain
exclusive access (and the ability to lend out the keys again, or
transfer ownership elsewhere, or even destroy Christine).
</div>

```rust
fn borrow_the_car_3() {
    let mut christine = Car::new();
    {
        let car_keys = &mut christine;
        let arnie = invite_friend_over();
        arnie.lend(car_keys);

        // Cannot use `christine` while mutably borrowed.

    } // end of scope for `arnie` and `car_keys`

    // We can again access `christine` here
}
```

(*): Sadly, return of "car keys" only a Rust guarantee, not physical world

## Shared Access {.center}

### Back to the world of computers

## Shared Access with `&T`

Many consumers do not require exclusive access to referenced data

 * validate a string input

 * search a table for the entry for some key

 * allocate new (unbalanced) tree from references to children

 * update some state *explicitly exposed* for shared mutation

Such cases are what `&T` is for

## `&T` example: string validation

Problem: validate a string input to ensure it is a "good" password.

```rust
enum ValidationError { NeedDigit, NeedNonAlphaNum }

fn validate(s: &str) -> Result<(), ValidationError> {
    let has_digit = s.chars()
        .any(|c| match c { '0'...'9' => true, _ => false });
    let has_nonalnum = s.chars()
        .any(|c| match c { '0'...'9' |
                           'a'...'z' |
                           'A'...'Z' => false,
                           _ => true });

    if !has_digit {
        Err(ValidationError::NeedDigit)
    } else if !has_nonalnum {
        Err(ValidationError::NeedNonAlphaNum)
    } else {
        Ok(())
    }
}
```

## `&T` example: exposed mutable state

Test-driven demonstration:

```rust
use std::cell::Cell;
use std::ops::Range;

struct TrackMaxSeen {
    v: Vec<i64>,
    last_read: Cell<Option<i64>>,
}

#[test]
fn demo_sum_range() {
    let tms = TrackMaxSeen::new(vec![1, 2, 3, 4]);
    assert_eq!(sum_range(&tms, 2..4), 7);
    assert_eq!(sum_range(&tms, 0..2), 3);
    assert_eq!(tms.last_read.get(), Some(2));
}
```

----

```rust
impl TrackMaxSeen {
    fn new(v: Vec<i64>) -> Self {
        TrackMaxSeen { v: v, last_read: Cell::new(None) }
    }
}

fn sum_range(tms: &TrackMaxSeen, range: Range<usize>) -> i64 {
    let mut sum = 0;
    let mut local_last_read = None;
    for &elem in &tms.v[range] {
        sum += elem;
        local_last_read = Some(elem);
    }
    if local_last_read.is_some() {
        tms.last_read.set(local_last_read);
    }
    return sum;
}
```




## TODO

Library "smart-pointer" types
