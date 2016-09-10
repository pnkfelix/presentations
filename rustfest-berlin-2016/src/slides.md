% Subtyping in Rust and Clarke's Third Law
% Felix Klock (`@pnkfelix`), Mozilla
% Rust Fest, Berlin, 17 September 2016

# Bienvenue {.center}

```rust
#![allow(dead_code, unused_variables)]
```

## Advanced Technology and Magic

Invited to give technical talk

Lots of interesting tech in Rust

* Borrow Checker

* Trait and Object System

* Destructor Semantics

* L-value `match`-ing

----

```art
.---.
|   |  Holy
'-+-'         Rectangle
  |                       Batman
  |   .----.  +-------+  .------.
  |   |    |  |       |  |      |
  '-->|    +->|       +->|      |
      |    |  |       |  |      |
      '----'  +-------+  '------'
```

```rust
fn here_is() {
    struct Some { rust: u32 }
    let s = Some { rust: 10 };
}
```

## Aside: On Magicians and Lying

or Aside: Are Magicians Liars?

# Why might we care about subtyping?

# What is Subtyping?

## Intuition: "Compatibility"

* "Have a `&mut Vec<T>`, but this function wants a `&[T]` slice..."

* "Have a `String`, but this method is only defined on `str` ..."

* "Want to return `Err(IOErr)` but the return type is `Result<(), ParseErr>` ..."

. . .

Amazingly "try it and see" often works.

## Exploring References

The reference types `&mut T` and `&T` look like candidates for
having some sort of compatibility relationship.

"Have a `&mut [i32]`, but code expects a `&[i32]` slice. What now?"

### Try it and see

. . .

```rust
fn product(nums: &[i32]) -> i32 {
    let mut x = 1;
    for n in nums { x *= *n; }
    x
}
```

. . .

```rust
fn demo_product(m: &mut [i32]) {
    m[0] = product(m);
}
```

. . .

```art
             m: &mut [i32]
             |
             v
fn product(&[i32])
```

Maybe we can plug in a `&mut [i32]` whenever we want a `&[i32]`

## Exploring Vectors and Slices

`Vec<T>` and `[T]` strike me as "looking like subtyping".

```rust
fn rotate(nums: &mut [i32]) {
    let len = nums.len();
    let first = nums[0];
    for i in 1..len { nums[i-1] = nums[i]; }
    nums[len-1] = first;
}
```

"Have a `Vec<i32>`, but code expects a `[i32]` slice. What now?"

### Try it and see

. . .

``` {.rust .compile_error .stripes}
fn demo_rotate() {
    let mut v = vec![1, 2, 3];
    rotate(v);
    assert_eq!(v, &[3, 1, 2]);
}
```

. . .

```rust
fn demo_rotate() {
    let mut v = vec![1, 2, 3];
    rotate(&mut v);
    assert_eq!(v, &[3, 1, 2]);
}
```

## Observation {.center}

Maybe we can plug in `&mut Vec<T>` whenever we want a `&mut [T]`

. . .

Q: Can we conclude `&mut Vec<T>` is a subtype of `&mut [T]`?

. . .

A: (we'll see; we certainly have seen something interesting linking them.)

## More experiments

Assume `T` is some type

"Have an X, but code expects a Y",<br/>
for every X,Y from `{ T, &T, &mut T }`

Let us try "something simple": `char` arrays.

## Magical receivers

Review: Extending types with new methods

```rust
trait AsciiIncr {
    /// Increments `self` by one unit. (Only works for 7-bit ASCII characters though.)
    fn incr(&mut self);
}

impl AsciiIncr for char {
    fn incr(&mut self) { *self = (*self as u8 + 1) as char }
}

#[test]
fn check_ascii_incr() {
    let mut c: char = 'a';
    c.incr();
    assert_eq!(c, 'b');
}
```

. . .

Q: Why not just use this function, instead of messing with traits?
```rust
fn ascii_incr(c: &mut char) { *c = (*c as u8 + 1) as char }
```


## Exploring one's `self`

Exploration: Mock up trivially simple traits, rather than get bogged
down with code of "useful" traits (like `AsciiIncr`).

## Sanity Check { data-transition="fade-out" }

```rust
trait Receiver {
    fn by_ref(&self); fn by_mut(&mut self); fn by_val(self) where Self: Sized;
}

impl Receiver for [char; 2] {
    fn by_ref(&self)     { println!("ref: {:?}", self[0]); }
    fn by_mut(&mut self) { println!("mut: {:?}", self[0]); self[1].incr(); }
    fn by_val(mut self)  { println!("val: {:?}", self[0]); self[1].incr(); }
}
```

. . .

```rust
#[test]
fn demo_obvious_cases() {
    let a: [char; 2]      =      ['a', '1'];
    let b: &[char; 2]     =     &['b', '4'];
    let c: &mut [char; 2] = &mut ['c', '7'];

    // [char; 2]   &[char; 2]     &mut [char; 2]
    a.by_val();
                   b.by_ref();
                                  c.by_mut();
    println!("obvious: (a,b,c): {:?}", (a,b,c));
}
```

prints:

. . .

```
obvious: (a,b,c): (['a', '1'], ['b', '4'], ['c', '8'])
```

## Exploration { data-transition="fade" }

``` {.rust}
trait Receiver {
    fn by_ref(&self); fn by_mut(&mut self); fn by_val(self) where Self: Sized;
}

impl Receiver for [char; 2] {
    fn by_ref(&self)     { println!("ref: {:?}", self[0]); }
    fn by_mut(&mut self) { println!("mut: {:?}", self[0]); self[1].incr(); }
    fn by_val(mut self)  { println!("val: {:?}", self[0]); self[1].incr(); }
}
```
. . .

```rust
#[test]
fn demo_interesting_cases() {
    let mut a: [char; 2]  =      ['a', '1'];
    let b: &[char; 2]     =     &['b', '4'];
    let c: &mut [char; 2] = &mut ['c', '7'];

    // [char; 2]   &[char; 2]     &mut [char; 2]
    a.by_val();    b.by_val();    c.by_val();
    a.by_ref();    b.by_ref();    c.by_ref();
    a.by_mut();    /*  ...  */    c.by_mut();
    println!("interesting: (a,b,c): {:?}", (a,b,c));
}
```

. . .

Only `b.by_mut()` rejected by compiler.

## Observation {.center}

For receiver for a method (i.e. the `x` in `x.m(...)`):

 * only going from `&[char; n]` to `&mut [char; n]` is disallowed

 * can go from `[char; n]` to `&mut [char; n]`, `&[char; n]` to `[char; n]`, etc.

. . .

Does this make sense?

(How can we go from a reference to a value?)


## ~~> in parameter

yes SOMETHING SOMETHING

but not SOMETHING

nor SOMETHING

## ~~> in `Result::Err` (`?`)

"Have a specific error, but result's error is more general. What now?"

. . .

### Try it and see

``` {.rust}
fn phase_1(x: i32) -> Result<i32, PhaseOneErr> { /* ... */ }
fn phase_2(y: i32) -> Result<i32, PhaseTwoErr> { /* ... */ }

fn composed(a: i32) -> Result<i32, EndToEndErr> {
    let b = try!(phase_1(a));
    let c = try!(phase_2(b));
    return Ok(c);
}
```

## also, trait tricks

e.g. `IntoPath`, `AsStr`

# Reality

## What is actually happening?

## Coercions, Borrowing, Protocols

# Coercions

## Receiver coercion

e.g. `self`, `&self` `&mut self`

auto-ref

auto-deref

## Deref coercion

## Auto Borrowing and Reborrowing

(imm and mut)

## Protocols

How did we get `EndToEndErr` from the results of `phase_1`/`phase_2`?

. . .

``` {.rust}
fn phase_1(x: i32) -> Result<i32, PhaseOneErr> { /* ... */ }
fn phase_2(y: i32) -> Result<i32, PhaseTwoErr> { /* ... */ }

fn composed(a: i32) -> Result<i32, EndToEndErr> {
    let b = try!(phase_1(a));
    let c = try!(phase_2(b));
    return Ok(c);
}
```

. . .

Are `PhaseOneErr` and `PhaseTwoErr` subtypes of `EndToEndErr`?

. . .

Answer: Magic is hidden behind the `try!`

----

Answer: Magic is hidden behind the `try!`

``` {.rust}
    let b = try!(phase_1(a));
```

expands to:

``` {.rust}
    let b =
        match phase_1(a) {
            ::std::result::Result::Ok(val) => val,
            ::std::result::Result::Err(err) => {
                return ::std::result::Result::Err(::std::convert::From::from(err))
            }
        };
```

which means that its inserting a conversion via `std::convert::From`
to transform your specific error to whatever error is expected
in the context of that expression.

FIXME: `?`/`try!`-protocol

FIXME: `From`/`Into`

## Gotchas re: coercions

Compiler needs the expected type.

E.g. this compiles and runs:
```rust
fn process1(input: &[i32]) { }
fn foo() { process1(&vec![1, 2, 3]); }
```

but this fails at compile time:

``` {.rust .compile_error }
trait Input { }
impl Input for [i32] { }
fn process2<I>(input: &I) where I: Input { }
fn bar() { process2(&vec![1, 2, 3]); }
```

## Why?


``` {.rust}
fn process1(input: &[i32]) { }
```

```art
         &Vec<i32>
            |
            v
 process1(&[i32])
```

Compiler sees input + expected types

 * adds `&Vec -> &[i32]` coercion

. . .

``` {.rust .compile_error }
fn process2<I>(input: &I) where I: Input { }
```

```art
         &Vec<i32>
            |
            v
   process2(&I)
```

Compiler decides `I` is `Vec<i32>`

 * tries to find `impl Input for Vec<i32>`


# Yes, but is it subtyping?

## (Why wouldn't it be?)

## Caveat

One *can* apply a coercion semantics to subtyping in a programming language.

See Pierce, section 15.6.

## A testing method

## Gentzen-style subtyping deduction rules

```art
     Y <: X
-----------------
A -> Y <: A -> X
```

aka `->` is covariant with respect to its return type.
(FIXME: Or is that stated the other way around?)

## Covariance intuition

If I hand a oven-ready apple pie to someone wanting a ready-to-bake
dessert, all is well.

```art
        ApplePie <: Dessert
------------------------------------
Heat -> ApplePie <: Heat -> Dessert
```

## Covariance in other programming languages

Java TODO

## So: Does ~~> generalize the same way?

code examples

## Previous Examples are not Subtyping

## Why might it matter if Rust has Subtyping?

Java example: Does this compile?

```java
    static void modify_array(Number[] numArray) {
        numArray[0] = new Float(3.14);
    }
```

. . .

What happens here?

<!-- FIXME consider adding *caution* signal orange CSS here. -->

```java
    static void mut_array() {
        Integer[] intArray = new Integer[1];
        modify_array(intArray);
    }
```

. . .

```
Exception in thread "main" java.lang.ArrayStoreException: java.lang.Float
    at Examples.modify_array(Examples.java:25)
    at Examples.mut_array(Examples.java:21)
    at Examples.main(Examples.java:9)
```

## The Rust Analogue

```art
                  +----------+
                  |  Number  |
                  +-----+----+
                       / \
                      +-+-+
                        |
             +----------+-----------+
             |                      |
        +----------+          +-----------+
        |  Float   |          |  Integer  |
        +----------+          +-----------+
```

. . .

What should be legal? This?

``` {.rust}
fn store_float_in_num_dest(num_dest: &mut Number, flo: Float) {
    *num_dest = flo;
}
```

. . .

If so, how about this?

``` {.rust}
fn pass_int_dest_as_num_dest(int: Integer) {
    let mut my_int: Integer = int;
    store_float_in_num_dest(&mut my_int);
}
```

## Q: Does Rust have Subtyping?

. . .

Unequivocally: Yes

## Yes, Rust has Subtyping

```rust
/// Picks either `x` or `y`, based on some internal choice.
fn pick<'a>(x: &'a i32, y: &'static i32) -> &'a i32 {
    if *x > 0 { x } else { y }
}

static GLOBAL: i32 = 100;

#[test] fn pick_test() { let temp: i32 = 200; pick(&temp, &GLOBAL); }
```

```art
 |
 : 'static (`*y` guaranteed to live at least this long)
 :
 :    .------.
 :    |      |
 :    |      |    |'a (`*x` guaranteed to live at least this long)
 :    | temp |    :
 :    |      |    |
 :    |      |
 :    '------'
 :
 |
```

. . .

The value `y: &'static i32` is able to be returned as a `&'a i32`.

This is sound because `'static` outlives `'a`

  * (syntax: `'static: 'a`, pronounced "outlives")

## And the other direction?

``` {.rust .compile_error }
fn promote<'a>(x: &'a i32) -> &'static i32 {
    return x;
}

#[test] fn promote_test() { let temp: i32 = 200; promote(&temp); }
```
```art
 |
 : 'static (`fn promote` promises a reference valud for this long)
 :
 :    .------.
 :    |      |
 :    |      |   |'a (`*x` only promises to live this long)
 :    | temp |   :
 :    |      |   |
 :    |      |
 :    '------'
 :
 |
```

. . .

Not legal to return `&'a i32` as a `&'static i32` for arbitrary `'a`;
otherwise, dangling pointers.

## General rules

`&'static T <: &'a T`, for any `'a` and any `T`.

(E.g., can pass `&'static &'static i32` when a `&'a &'static i32`
 expected, by plugging in `&'static i32` for `T`.

. . .

A more general rule:

```art
   'b : 'a
--------------
&'b T <: &'a T
```

. . .

Even *more* general:

```art
   'b : 'a
    S <: T
--------------
&'b S <: &'a T
```
. . .

i.e., can pass `&'c &'static i32`
when `&'c &'d i32` expected.

. . .

> "Isn't this obvious, that I can pass long lived references
> where short-lived references are expected? When is type
> theory going to be useful?"

## Details matter

So far, only looked at immutable references `&'a T`.

. . .

What are rules for `&'a mut T`?

----

Consider these two function signatures

```rust
fn non_store<'a>(dest1: &'a     &'a i32, ptr: &'a i32) { /* ... */ }
fn may_store<'a>(dest2: &'a mut &'a i32, ptr: &'a i32) { /* ... */ }
```

. . .

`fn non_store` only gets immutable ref's; safe to call it like this:

```rust
#[test] fn non_store_test() {
    let long_ten = 10;
    let mut long_ptr = &long_ten;
    let four = 4;
    let local = &four;
    non_store(&mut long_ptr, local);
}
```

. . .

`fn may_store` has more expressive freedom. Which means:

``` {.rust .compile_error}
#[test] fn may_store_test() {
    let long_ten = 10;
    let mut long_ptr = &long_ten;
    let four = 4;
    let local = &four;
    may_store(&mut long_ptr, local);
}
```

```
error: `four` does not live long enough
```

## `four` doesn't live long enough

``` {.rust}
fn non_store<'a>(dest1: &'a     &'a i32, ptr: &'a i32) { /* ... */ }
fn may_store<'a>(dest2: &'a mut &'a i32, ptr: &'a i32) { /* ... */ }
```

```art
    let long_ten = 10;                ----------------------.
    let mut long_ptr = &long_ten;     ---------.            | 'long_ten
    let four = 4;                     -.       | 'long_ptr  |
    let local = &four;                 |'four  |            |
    may_store(&mut long_ptr, local);   |       |            |
                                      -'      -'           -'
```

A call to `non-store` can choose `'a` to be as short as the lifetime
of `four`, and then reason:

```art
            'long_ptr : 'four
       ----------------------------
       &'long_ptr i32 <: &'four i32
------------------------------------------ covariance of &T wrt T
&'four &'long_ptr i32 <: &'four &'four i32
```

But the call to `may_store` cannot employ the same reasoning!

. . .

To have `&'b mut S <: &'a mut T`, the types `S` and `T` must be *equal*.

----

To have `&'b mut S <: &'a mut T`, the types `S` and `T` must be *equal*.

. . .

I.e. core rule for subtyping of `&mut` is:

```art
        'b : 'a
----------------------
&'b mut T <: &'a mut T
```
. . .

This is the heart of why "variance" matters in Rust

`&T` is *covariant* with respect to `T`, while
`&mut T` is *invariant* with respect to `T`

<!--
FIXME: maybe verify that a reborrow is what else is happening here,
and not "just" subtyping between `&mut` and `&`. But maybe just gloss
over that detail.
-->

----

```rust
#[test] fn no_store_test() {
    fn no_store<'a>(dest: &'a &'a i32, ptr: &'a i32) {

    }

    static HUNDRED: i32 = 100;

    fn helper<'long>(long_dest: &'long mut &'long i32) {
        let temp: i32 = 13;
        let short_ptr = &temp;
        no_store(long_dest, short_ptr);
    }

    let mut long_ptr: &i32 = &HUNDRED;
    helper(&mut long_ptr);
}
```

```art
HUNDRED <--------+
                 |
  =+='long  .----+------. <--------+
   :        |    |      |          |
   :        |    |      |    .-----+-----.
   :        |    |      |    |     |     |
   :        |           |    |           |  .------. <-------+
   :        | long_ptr  |    | long_dest |  |      |         |
   :        |           |    |           |  | temp |   .-----+-----.
   :        |           |    |           |  |      |   |     |     |     |'a
   :        |           |    |           |  |      |   | short_ptr |     :
   :        |           |    |           |  |      |   |           |     |
   :        |           |    |           |  |      |   |           |
   :        |           |    |           |  |      |   '-----------'
   :        |           |    |           |  '------'
   :        |           |    |           |
   :        |           |    '-----------'
   :        |           |
  =+=       '-----------'
```


----

``` {.rust .compile_error}
#[test] fn do_store_test() {
    fn do_store<'a>(dest: &'a mut &'a i32, ptr: &'a i32) {
        *dest = ptr;
    }

    static HUNDRED: i32 = 100;

    fn helper<'long>(long_dest: &'long mut &'long i32) {
        let temp: i32 = 13;
        let short_ptr = &temp;
        do_store(long_dest, short_ptr);
    }

    let mut long_ptr: &i32 = &HUNDRED;
    helper(&mut long_ptr);
}
```


## The Sapin example

Let's try to implement `std::cell::Cell`, in user code.

```rust
struct MyCell<T: Copy> {
    value: T,
}

impl<T: Copy> MyCell<T> {
    fn new(x: T) -> MyCell<T> { MyCell { value: x } }
    fn get(&self) -> T { self.value }
    fn set(&self, value: T) {
        use std::ptr;
        unsafe {
            ptr::write(&self.value as *const _ as *mut _, value);
        }
    }
}
```

Is this use of `unsafe` sound?

----

```rust
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);

    fn step1<'a>(r_c1: &MyCell<&'a i32>) {
        let val: i32 = 13;
        step2(&val, r_c1);
        println!("step1 r_c1.value: {}", r_c1.value);
    }

    fn step2<'a>(r_val: &'a i32, r_c2: &MyCell<&'a i32>) {
        r_c2.set(r_val);
    }

    println!("  end cell.value: {}", cell.value);
}
```

. . .

Output:

```
step1 r_c1.value: 13
  end cell.value: 28672
```

##  { data-transition="fade-out" data-transition-speed="fast" }

``` {.rust}
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);
    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'a>(r_val: &'a i32, r_c2: &MyCell<&'a i32>) { r_c2.set(r_val); }
}
```
. . .

before `r_c2.set(r_val)`:

```art
                             (static data area)

                      .----> X: 10
                      |
 .-> cell = MyCell    |
 |          value  ---'
 |   ----
 |
 +-- r_c1
 |
 |    val  13 <-------.
 |                    |
 |   ----             |
 |                    |
 |  r_val ------------'
 |
 '-- r_c2
```

## { data-transition="fade" data-transition-speed="fast" }

``` {.rust}
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);
    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'a>(r_val: &'a i32, r_c2: &MyCell<&'a i32>) { r_c2.set(r_val); }
}
```

after `r_c2.set(r_val)`:

```art
                             (static data area)

                             X: 10

 .-> cell = MyCell
 |          value  ---.
 |   ----             |
 |                    |
 +-- r_c1             |
 |                    |
 |    val  13 <-------+
 |                    |
 |   ----             |
 |                    |
 |  r_val ------------'
 |
 '-- r_c2
```

## { data-transition="fade" data-transition-speed="fast" }

``` {.rust}
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);
    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'a>(r_val: &'a i32, r_c2: &MyCell<&'a i32>) { r_c2.set(r_val); }
}
```

after `step2` returns:

```art
                             (static data area)

                             X: 10

 .-> cell = MyCell
 |          value  ---.
 |   ----             |
 |                    |
 '-- r_c1             |
                      |
      val  13 <-------'
```

## { data-transition="fade-in" data-transition-speed="fast" }

``` {.rust}
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);
    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'a>(r_val: &'a i32, r_c2: &MyCell<&'a i32>) { r_c2.set(r_val); }
}
```

after `step1` returns:

```art
                             (static data area)

                             X: 10

     cell = MyCell
            value  ---.
                      |
                      |
                      |
                      |
      ???     <-------'
```

## Rust certainly has subtyping

`&'static T <: &'x T`

. . .

More generally:

If `'a` outlives `'b`, then `&'a T <: &'b T`.

----

demo compatibilty

----

demo covariance

## Why does variance matter?

Getting covariance vs invariance right matters for soundness


## Pop Quiz

* `&'static mut T` (?) `&'a mut T`

* `&'static &'a mut T` (?) `&'a &'a mut T`

* `&'a mut &'static T` (?) `&'a mut &'a T`

## Why does variance matter


# Conclusion  {.center}

## More Info

## Thanks!
