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

# A Metaphor {.center}

## (reminder: metaphors never work 100%) {.big_text .center}

----

``` {.rust}
let christine = Car::new();
```

This is "Christine"

```art

                                      .---------------.
                                     /  .--------. .-. \
                                    /  /         | |  \ \
                                   /  /          | |   \ \
                                  /  /           | |    \ \
                                 /  /            | |     \ \
          .---------------------'  '-------------' '------' '-----.
         /                                                         \[car]
        /  ---                                                      \
       /  ----                                                       \
      /  -----                                                        \
     /                                                                 \
    '-------------------------------------------------------------------'
             \      /                                      \      /
              '----'                                        '----'

[car]: stroke="red"
```

pristine unborrowed car

<!-- ![pristine unborrowed car](christine_car_pristine.png) -->

(apologies to Stephen King)

----

``` {.rust}
let read_only_borrow = &christine;
```

```art
              .----.   -.             .---------------.
              | @ @|     \           /  .--------. .-. \
              |    |      \         /  /         | |  \ \
              '-+--'       \       /  /          | |   \ \
                |           \     /  /           | |    \ \
             +--+--+         \   /  /            | |     \ \
          .-- \ |   \         +-'  '-------------' '------' '-----.
         /   ------- \ -------                                     \[car]
        /  ---  [h]    [h2]                                         \
       /  ----                                                       \
      /  -----                                                        \
     /                                                                 \
    '-------------------------------------------------------------------'
             \      /                                      \      /
              '----'                                        '----'

[car]: stroke="red"
[h]: stroke="red"
[h2]: stroke="red"
```

single inspector (immutable borrow)

(apologies to Randall Munroe)

<!-- ![single inspector (immutable borrow)](christine_car_single_inspector.png) -->

----

``` {.rust}
read_only_borrows[2] = &christine;
read_only_borrows[3] = &christine;
read_only_borrows[4] = &christine;
```

```art
              .----.   -.             .---------------.
              | @ @|     \           /  .--------. .-. \
              |    |      \         /  /         | |  \ \
    .---      '-+--'.----. \       /  /          | |   \ \
    | @@        |   |    |  \     /  /           | |    \ \
    |   |    +--+--+|    |   \   /  /            | |     \ \
     +--' .-- \ | \ '--+-' \  +-'  '-------------' '------' '-----.
    /    /   ------\    \   \                                      \[car]
 +-+--+ /  --- [h]  +----+---+                                      \
 |    |/  ----      |        |                                       \
 |    /  -----      |        |                                        \
 |   /              |        |                                         \
 +- '---------------|        |------------------------------------------'
  |   [car2] \      +--------+                             \      /
  |           '----'  |    |                                '----'
                      |    |
[car]: stroke="red"
[car2]: stroke="red"
[h]: stroke="red"
[h2]: stroke="red"
```

many inspectors (immutable borrows)

<!-- ![many inspectors (immutable borrows)](christine_car_many_inspectors.png) -->

----

When inspectors are finished, we are left again with:

```art

                                      .---------------.
                                     /  .--------. .-. \
                                    /  /         | |  \ \
                                   /  /          | |   \ \
                                  /  /           | |    \ \
                                 /  /            | |     \ \
          .---------------------'  '-------------' '------' '-----.
         /                                                         \[car]
        /  ---                                                      \
       /  ----                                                       \
      /  -----                                                        \
     /                                                                 \
    '-------------------------------------------------------------------'
             \      /                                      \      /
              '----'                                        '----'

[car]: stroke="red"
```

pristine unborrowed car

<!-- ![pristine unborrowed car](christine_car_pristine.png) -->

----

``` {.rust}
let mutable_borrow = &mut christine; // like taking keys ...
give_arnie(mutable_borrow); // ... and giving them to someone
```

```art

                                      .---------------.
                                     /  .--------. .-. \
                                    /  / ---.    | |  \ \
                                   /  / |@@ |    | |   \ \
                                  /  /  |~~ |[d] | |    \ \
                                 /  /  /+-|-'    | |     \ \
          .---------------------'  '--+---+------' '------' '-----.
         /                                                         \[car]
        /  ---                                                      \
       /  ----                                                       \
      /  -----                                                        \
     /                                                                 \
    '-------------------------------------------------------------------'
             \      /  / / /                               \      / / / /
              '----'                                        '----'

[car]: stroke="red"
```

driven car (mutably borrowed)

<!-- ![driven car (mutably borrowed)](christine_car_driven.png) -->

## Can't mix the two in safe code! {.center}

<!--
------------------------------------------------------------------------- ----------------------------------------------------------
![many inspectors (immutable borrows)](christine_car_many_inspectors.png) ![driven car (mutably borrowed)](christine_car_driven.png)
------------------------------------------------------------------------- ---------------9-------------------------------------------
-->

### Otherwise: (data) races!

----

``` {.rust .compile_error}
read_only_borrows[2] = &christine;
let mutable_borrow = &mut christine;
read_only_borrows[3] = &christine;
// ⇒ CHAOS!
```

```art

                                      .---------------.
                                     /  .--------. .-. \
                                    /  / ---.    | |  \ \
                                   /  / |@@ |    | |   \ \
                                  /  /  |~~ |[d] | |    \ \
                                 /  /  /+-|-'    | |     \ \
          .---------------------'  '--+---+------' '------' '-----.
         /                                                         \[car]
        /  ---                                                      \
       /  ----                                                       \
      /  -----                                                        \
 .---.                                .----.                           \
 |   |------------------------------- |    |----------------------------'
||x x|---+   \      /  [car2]     +---| x x|-+             \      / / / /
|'---'    \   '----'           +--|   '----'  \--+          '----'
+----------+                  /   +------------+  \
[car]: stroke="red"
[car2]: stroke="red"
```

mixing mutable and immutable is illegal

<!-- ![mixing mutable and immutable is illegal](christine_car_driving_over_inspectors.png) -->

## {.center}

----------------- -------- -------------
Ownership         `T`
Exclusive access  `&mut T` ("mutable")
Shared access     `&T`     ("read-only")
----------------- -------- -------------

# Exclusive access {.center}

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

. . .

Owner loses capabilities attached to `&mut`-borrows only
*temporarily* (*)

<div class="notes">
Keys *will* return after `&mut`-borrow ends; I, the owner, will regain
exclusive access (and the ability to lend out the keys again, or
transfer ownership elsewhere, or even destroy Christine).
</div>

(*): "Car keys" return guaranteed by Rust; sadly, not by physical world

## End of metaphor

### (on to models)
