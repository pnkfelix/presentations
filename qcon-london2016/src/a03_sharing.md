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
-----------------------  ------------------ -------------------------
`Vec<T>`, `String`, ...  `i32`, `char`, ... `[T; n]`, `(T1, T2, ...)`

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

## Todo Code Link

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

*Transfer* access; *Arnie* never gets keys back! (details at linked code)

``` {.rust .compile_error}
    arnie.partner.take(k); k.color = arnie.fav_color;
    //                     ~~~~~~~~~~~~~~~~~~~~~~~~~
    // error: cannot assign to `k.color` because it is borrowed
```

<!-- Not sure this snippet pays for itself so commenting out for now

``` {.rust .compile_error}
impl<'b> ArnieLongTermRelationship<'b> {
    fn lend_3<'b>(&self, k: &'b mut Car) {
        self.partner.take(k); k.color = arnie.fav_color;
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

(*): "Car keys" return guaranteed by Rust; sadly, not by physical world

# Shared Access

## Shared Access {.center}

### Back to the world of computers

## Shared Access with `&T`

Many consumers do not require *exclusive* access to referenced data

 * validate a string input

 * search a table for the entry for some key

 * allocate new (unbalanced) tree from references to children

 * update some state *explicitly exposed* for shared mutation

Shared references `&T` are for such cases

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

Test-driven demonstration

```rust
use std::cell::Cell;

struct TrackMaxSeen {
    v: Vec<i64>,
    last_read: Cell<Option<i64>>,
}

#[test]
fn demo_tms_sum_range() {
    let tms = TrackMaxSeen { v: vec![1, 2, 3, 4],
                             last_read: Cell::new(None) };
    assert_eq!(tms.last_read.get(), None);
    assert_eq!(sum_range(&tms, 2..4), 7);
    assert_eq!(sum_range(&tms, 0..2), 3);
    assert_eq!(tms.last_read.get(), Some(2));
}
```

No `mut` in sight! (We will revisit this in a moment.)

## Ownership patterns

Single owner: `T`, `Box<T>`, collections, arenas

```rust
fn single_owner_demo() {
    let data = ['h','e','l','l','o'];           // ⇐ stack-allocated
    let boxed: Box<[char; 5]> = Box::new(data); // ⇐ heap reference
    let vector: Vec<char> = vec!['h','e','l','l','o'];
    // Heap storage automaticaly deallocated at end of owner's scope
}
```

## Ownership patterns II

Shared ownership: `Rc<T>`

<!--
```rust
fn helper<T>(x: T) {}
```
-->

```rust
#[test]
fn shared_owner_demo_1() {
    let r_one = Rc::new(vec!['h','e','l','l','o']);
    let r_two = r_one.clone();
    assert_eq!(r_one[0], 'h'); // ⇐ &-access is allowed
    helper(r_two.clone()); // ⇐ re-share ownership with helpers
}
```

Since ownership is shared, once we have an `Rc<T>`, mus conservatively
assume it is shared; implies we cannot get `&mut`-ref out of an `Rc`.

## Ownership patterns III

```rust
#[test]
fn shared_owner_demo_2() {
    let data = [Cell::new('h'), Cell::new('o')];
    let r_one: Rc<[Cell<char>; 2]> = Rc::new(data);
    let r_two = r_one.clone();
    assert_eq!(r_one[0].get(), 'h'); // ⇐ &-access is allowed
    r_two[0].set('a');               // thus we can set `Cell` elements
    assert_eq!(r_one[0].get(), 'a'); // ⇐ and observe it via other owners
}
```

... again no `mut` in sight!

<!--
```rust
use std::ops::Range;

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
-->
