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

#### Generic Types, Bounded Polymorphism

<!--
```rust
use std::ops::Add;
```
-->

```rust
fn choose<T>(x: T, y: T) -> T { return x; }

fn sum<T: Add>(x: T, y: T) -> T::Output { return x + y; }

fn sum3<T>(x: T, y: T, z: T) -> T where T: Add<Output=T> { return x + y + z; }
```

. . .

Type Constructions

Move                     Copy               Copy if `T:Copy`
-----------------------  ------------------ ----------------------------
`Vec<T>`, `String`, ...  `i32`, `char`, ... `[T; n]`, `(T1,T2,T3)`, ...

. . .

References to data

 * `&mut T`, `&T`

 * (plus library reference types: `Box<T>`, `Cow<T>`, `Rc<T>`, ...)

## Why multiple `&`-reference types? {.center}

 * Distinguish *exclusive* access from *shared* access

 * Enables **safe**, **parallel** API's

. . .

----------------- -------- -------------
Ownership         `T`
Exclusive access  `&mut T` ("mutable")
Shared access     `&T`     ("read-only")
----------------- -------- -------------

# A Metaphor {.center}

## `&mut`: can I borrow the car?

<div class="notes">
If I own a car, and I lend the keys to Arnie, I *still* own the car
</div>

<!--
```rust
use std::rc::Rc;
use std::cell::RefCell;
struct Car { color: Color, engine: Engine }
struct Arnie<GF=NoGirlfriend> { fav_color: Color, partner: GF }
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
impl Arnie {
    fn lend(&self, c: &mut Car) { lend_1(self, c); lend_2(self, c); }
}
fn lend_1(arnie: &Arnie, k: &mut Car) { k.color = arnie.fav_color; }
fn lend_2(arnie: &Arnie, k: &mut Car) {
    arnie.partner.lend(k); k.color = arnie.fav_color;
}
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
--->


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

```art
.-🚗
| ^
| : .-----.(scope of arnie)
| | |     |[scope]
| '-+-🔑   |
|   |     |
|   |     |
|   |     |
|   |     |
|   '-----'
|
'------------------------------> WORK
[scope]: stroke="blue"
```

-----

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

```art
.-🚗
| ^[ref]
| :
| : .-----.(scope of arnie)
| | |     |[scope]
| '-+-🔑   |
|   |     |
|   |     |
'---|-----|---------------------> WORK
[a] | [b] | [c]
    '-----'
[a]: stroke="red"
[b]: stroke="red"
[c]: stroke="red"
[scope]: stroke="blue"
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
