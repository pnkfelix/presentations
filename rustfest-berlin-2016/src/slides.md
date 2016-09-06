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

* "I have a `&mut Vec<T>`, but this function wants a `&[T]` slice..."

* "I have a `String`, but this method is only defined on `str` ..."

* "I want to return `Err(IOErr)` but the return type is `Result<(), ParseErr>` ..."

. . .

Amazingly "try it and see" often works.

## Exploring References

The reference types `&mut T` and `&T` look like candidates for
having some sort of compatibility relationship.

"I have a `&mut [i32]`, but code expects a `&[i32]` slice. What now?"

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

"I have a `Vec<i32>`, but code expects a `[i32]` slice. What now?"

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

A: (No, but we have seen something interesting linking them.)

## More experiments

Assume `T` is some type

"I have an X, but code expects a Y",<br/>
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

`IOErr` ~~> `ParseErr`

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

`IOErr` ~~> `ParseErr`

`?`/`try!`-protocol

`From`/`Into`

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

## Q: Does Rust have Subtyping?

## Demo

`&'static T <: &'x T`

compatibilty

----

covariance

## Why does variance matter?

Getting covariance vs invariance right matters for soundness

Java mutable array example

## Pop Quiz

* `&'static mut T` (?) `&'a mut T`

* `&'static &'a mut T` (?) `&'a &'a mut T`

* `&'a mut &'static T` (?) `&'a mut &'a T`

## Why does variance matter

The Sapin `Cell` example

# Conclusion  {.center}

## More Info

## Thanks!
