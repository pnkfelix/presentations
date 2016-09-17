## Subtyping in Rust and Clarke's Third Law

### Felix Klock (`@pnkfelix`), Mozilla Research
### Rust Fest, Berlin; 17 Sept 2016

Slides: [`http://bit.ly/2cMFbYB`](http://bit.ly/2cMFbYB)

<table>
<tr>
<td><kbd class="key">space</kbd>                              </td>
<td>next slide</td>
</tr>
<tr>
<td>
<kbd class="key">shift</kbd><kbd class="key">space</kbd>
</td>
<td>
previous slide
</td>
</tr>
<tr>
<td>
<kbd class="key">esc</kbd>
</td>
<td>
overview
</td>
</tr>
<tr>
<td>
arrows
</td>
<td>
navigate
</td>
</tr>
</table>

## Advanced Tech and Magic

> Any sufficiently advanced technology is indistinguishable from magic.
>
> Third Law, Arthur C. Clarke

. . .

> The only way of discovering the limits of the possible is to venture a little way
> past them into the impossible.
>
> Second Law, Arthur C. Clarke

. . .

(Clarke's [first law](https://en.wikipedia.org/wiki/Clarke%27s_three_laws) is an impossible statement about elderly scientists.)

## Language Technology

Lots of interesting tech in Rust

* Borrow Checker

* Trait and Object System

* Destructor Semantics

* L-value `match`-ing

. . .

Any of above can look like magic

. . .

But, *subtyping* has characteristics of a magic show

"We all already know what subtyping is..."

## Aside: On Magicians and Lying

> "You can lie as entertainment, but still be honest and moral"
>
> Penn Jillette, attributed to James Randi

. . .

I'm probably going to lie to you several times during this
presentation.

(Sometimes exploratory science, sometimes magic show.)

## Poll {.center .left_align}

Raise your hand if you ...

>- know what [http://play.rust-lang.org](http://play.rust-lang.org) is?

>- know difference between a slice of `T` (i.e. `&[T]`) and a `Vec<T>`?

>- know what `PhantomData<T>` is?

# What is subtyping? {.center}

## Why do we care about subtyping?

Insisting types of inputs exactly match expected types leads compiler
to reject programs that seem obviously well-behaved.

## Intuition: "Compatibility"

* "Have a `&mut Vec<T>`, but this function wants a `&[T]` slice..."

* "Have a `String`, but this method is only defined on `str` ..."

* "Want to return `Err(IOErr)` but the return type is `Result<(), ParseErr>` ..."

. . .

Amazingly, "try it and see" often works.

## Results and Errs

"Have a specific error, but result's error is more general. What now?"

### [Try it][result_err_gist] and see

[result_err_gist]: https://play.rust-lang.org/?gist=744b51178a92f8c15cf1dca226e08680

. . .

<!--
```rust
struct Ast;
struct Mir;
struct Out;
struct PhaseOneErr;
struct PhaseTwoErr;
fn phase_1(x: Ast) -> Result<Mir, PhaseOneErr> { unimplemented!() }
fn phase_2(y: Mir) -> Result<Out, PhaseTwoErr> { unimplemented!() }
```
-->

``` {.rust}
fn phase_1(x: Ast) -> Result<Mir, PhaseOneErr> { /* ... */ }
fn phase_2(y: Mir) -> Result<Out, PhaseTwoErr> { /* ... */ }
```

. . .

``` {.rust}
fn composed(a: Ast) -> Result<Out, EndToEndErr> {
    let b = try!(phase_1(a));
    let c = try!(phase_2(b));
    return Ok(c);
}
```

. . .

(we will look more at an instance of this example later)

## Exploring Vectors and Slices { data-transition="fade-out" .tight }

`Vec<T>` and `[T]` *look* like subtyping

"Have a `Vec<i32>`, but code expects a `[i32]` slice. What now?"

### [Try it][vec_slice_gist] and see

[vec_slice_gist]: https://play.rust-lang.org/?gist=9bfb91fd850713445f42c8ef607c9c10

. . .

```rust
fn rotate(nums: &mut [i32]) {
    let len = nums.len();
    let first = nums[0];                     // save first number
    for i in 1..len { nums[i-1] = nums[i]; } // shift up all other numbers
    nums[len-1] = first;                     // put saved number at end
}
```

. . .

```
fn demo_rotate() {
    let v = vec![1, 2, 3];
    rotate(v);
    assert_eq!(v, &[2, 3, 1]);
}
```

does it compile?

## Exploring Vectors and Slices { data-transition="fade-in" .tight }

`Vec<T>` and `[T]` *look* like subtyping

"Have a `Vec<i32>`, but code expects a `[i32]` slice. What now?"

### [Try it][vec_slice_gist] and see

``` {.rust}
fn rotate(nums: &mut [i32]) {
    let len = nums.len();
    let first = nums[0];                     // save first number
    for i in 1..len { nums[i-1] = nums[i]; } // shift up all other numbers
    nums[len-1] = first;                     // put saved number at end
}
```

``` {.rust .compile_error .stripes}
fn demo_rotate() {
    let v = vec![1, 2, 3];
    rotate(v);
    assert_eq!(v, &[3, 1, 2]);
}
```

. . .

<!--
```rust
#[test]
```
-->

```rust
fn demo_rotate() {
    let mut v = vec![1, 2, 3];
    rotate(&mut v);
    assert_eq!(v, &[2, 3, 1]);
}
```

. . .

## [Stripped][vec_slice_stripped] to skeletal form

[vec_slice_stripped]: https://play.rust-lang.org/?gist=eec18313e9e454a60b6859954b75f414

```rust
mod demo_vec_slice {
    fn provide(m: &mut Vec<i32>) { expect(m); }
    fn expect(_nums: &mut [i32]) { unimplemented!() }
}
```

```art
        &mut Vec<i32>
            |
            v
  fn expect(&[i32])
```

. . .

"If it compiles, it works"

## Exploring References

The reference types `&mut T` and `&T` look like candidates for
having some sort of compatibility relationship.

"Have a `&mut [i32]`, but code expects a `&[i32]` slice. What now?"

### [Try it][mut_imm_slice] and see

[mut_imm_slice]: https://play.rust-lang.org/?gist=536ad61bf7f63ffd654a47db80ecbd02
. . .

```rust
mod demo_mut_slice_imm_slice {
    fn provide(m: &mut [i32]) { expect(m); }
    fn expect(_nums: &[i32]) { unimplemented!() }
}
```

```art
         &mut [i32]
            |
            v
  fn expect(&[i32])
```

<!--
## Textbook subtyping { data-transition="fade-out" }

Classic example: "Records"

* "Have a record with these fields, want one with a subset of the fields"

* A Rust analogue: "Have `(A, B, C)`, this function wants `(A, B)`"

. . .

### Try it and see

```
mod demo_subtuple {
    fn provide<A, B, C>(tup: (A, B, C)) { expect(tup); }
    fn expect<A, B>(_tup: (A, B)) { unimplemented!(); }
}
```

does it compile?
-->

## Methodology and methods

If you have `self` or `Self`, you're (probably) talking about a method

. . .

"Have an X, but code expects a Y",<br/>
for every X,Y from `{ Self, &Self, &mut Self }`

Let us try "something simple": Methods on `char` arrays.

## Magical receivers

[Review][ext_types_via_trait]: Extending types with new methods

[ext_types_via_trait]: https://play.rust-lang.org/?gist=555b603e6931ba51e68a79b307a0517b

```rust
trait AsciiIncr {
    /// Increments `self` by one unit. (Only works for 7-bit ASCII characters though.)
    fn incr(&mut self);
}

impl AsciiIncr for char {
    fn incr(&mut self) { *self = (*self as u8 + 1) as char }
}
```

. . .

```rust
#[test]
fn check_ascii_incr() {
    let mut c: char = 'a';
    c.incr();
    assert_eq!(c, 'b');
}
```

<!--
. . .

Q: Why not just use this function, instead of messing with traits?
```rust
fn ascii_incr(c: &mut char) { *c = (*c as u8 + 1) as char }
```
-->

## Exploring one's self { data-transition="fade" }

Exploration: Mock up trivially simple traits, rather than get bogged
down with code of "useful" traits (like `AsciiIncr`).

. . .

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

## [Sanity check][sanity_check] { data-transition="fade" .tight }

[sanity_check]: https://play.rust-lang.org/?gist=ea5cc1973f3d867b1837fdd4b7d95e71

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

<!-- (save space on slide by putting directive into unrendered area.)
```rust
#[test]
```
-->
```rust
fn demo_obvious_cases() {
    let a = ['a', '1']; let b = &['b', '4']; let c = &mut ['c', '7'];
    // [char; 2]        &[char; 2]           &mut [char; 2]
    a.by_val();
                        b.by_ref();
                                             c.by_mut();
    println!("obvious: (a,b,c): {:?}", (a,b,c));
}
```
prints:

. . .

```
val: 'a'
ref: 'b'
mut: 'c'
obvious: (a,b,c): (['a', '1'], ['b', '4'], ['c', '8'])
```

## [Exploration][explore_recv] { data-transition="fade" }

[explore_recv]: https://play.rust-lang.org/?gist=db981260d7e9d9d2cc5c9454573b10e7

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
    let mut a = ['a', '1']; let b = &['b', '4']; let c = &mut ['c', '7'];
    // [char; 2]            &[char; 2]           &mut [char; 2]
    a.by_val();             b.by_val();          c.by_val();
    a.by_ref();             b.by_ref();          c.by_ref();
    a.by_mut();             /*  ...  */          c.by_mut();
    println!("interesting: (a,b,c): {:?}", (a,b,c));
}
```

. . .

Only `b.by_mut()` rejected by compiler.

## Observation {.center .left_align}

For receiver for a method (i.e. the `x` in `x.m(...)`):

 >- only going from `&[char; n]` to `&mut [char; n]` is disallowed
 >- can go from `[char; n]` to `&mut [char; n]`
 >- `&[char; n]` to `[char; n]`
 >- et cetera

. . .

Does this make sense?

(How can we go from a reference to a value?)

# Is this subtyping? {.center}

## View from OOP VM runtime hacker


```art
             .-------------------------.
             |  ptr: NonZero<*const T> |
             | ----------------------- |
Vec<T> repr: |  cap: usize             |
             | ----------------------- |
             |  len: usize             |
             '-------------------------'

             .-------------------------.
             | data: *const T          |
  &[T] repr: | ----------------------- |
             |  len: usize             |
             '-------------------------'
```

. . .

How could `Vec<T>` possibly be a subtype of `&[T]`?
They have incompatible representations.

. . .

(However, a language hacker's view may differ...)

## Is this subtyping? {.center}

(*Temporarily* putting aside previous examples)

## Textbook subtyping

Classic example: "Records"

* "Have a record with these fields, want one with a subset of the fields"

* A Rust analogue: "Have `(A, B, C)`, this function wants `(A, B)`"

### [Try it][demo_subtuple_err] and see

[demo_subtuple_err]: https://play.rust-lang.org/?gist=c0aaaee54247f2fd24aef5e1e48d2261

``` {.rust .compile_error .stripes}
mod demo_subtuple {
    fn provide<A, B, C>(tup: (A, B, C)) { expect(tup); }
    fn expect<A, B>(_tup: (A, B)) { unimplemented!(); }
}
```

. . .

(Q: Why might disallowing this be a good idea for a language like Rust?)

## Textbook subtyping {.left_align}

Academic example: Functions

* Assume types `Int`, `Real` where `Int <: Real`

```art
                .------------------------------------.
                |                                    |
                | Real …, -7.2, ⅓, ½, 3.14, π, …     |
                |                                    |
                |  .---------------------------.     |
                |  |                           |     |
                |  | Int …, -1, 0, 1, 2, 3, …  |     |
                |  |                           |     |
                |  '---------------------------'     |
                '------------------------------------'
```

* "Have a function `f` of type `Real -> Int`, want one like `Real -> Real`"

* i.e., is it legal to pass `f` as if it is a `Real -> Real`?

* What about a function `g` of type `Int -> Int`? Can that be passed as `Real -> Real`?

## Textbook subtyping {.left_align}

* "Have a function of type `Int -> Int`, want one like `Real -> Real`"

. . .

* Won't work: `g: Int -> Int` *assumes* input is always an `Int`, yet
  client taking `Real -> Real` is free to apply it to -7.2 or ⅓ or π.

```
twice: (Real -> Real) Real -> Real
twice(f, r) = f(f(r))

num_divisors: Int -> Int
num_divisors(4)  = |{1, 2, 4}| = 3
num_divisors(12) = |{1, 2, 3, 4, 6, 12}| = 6

twice(num_divisors, 2.3) = num_divisors(num_divisors(2.3))
                         = num_divisors(???)
                         = ill-defined!
```

* ~~`Int -> Int` <: `Real -> Real`~~

. . .

* "Have a function of type `Real -> Int`, want one like `Real -> Real`"


## Textbook subtyping {.left_align}

* ~~`Int -> Int` <: `Real -> Real`~~

* "Have a function of type `Real -> Int`, want one like `Real -> Real`"

. . .

```
twice: (Real -> Real) Real -> Real
twice(f, r) = f(f(r))

ceil_plus: Real -> Int
ceil_plus(x) = ⌈x⌉ + 1
```

. . .

```art
      ceil_plus   2.3: Real
       |           |
       v           v
twice( f,           r)  = ceil_plus(ceil_plus(2.3)) = ceil_plus(⌈2.3⌉ + 1) = ⌈4⌉ + 1 = 5
```

* This makes sense, right? (Assume compatible runtime representation.)

. . .

"Client says they can handle a production of any real number, so it is safe to provide
something that will only produce integer values."

## Textbook subtyping {.left_align data-transition="fade" }

* "Have a function of type `Real -> Int`, want one like `Real -> Real`"

"Client says they can handle a production of any real number, so it is safe to provide
something that will only produce integer values."

. . .

```java
class Real             { Real increment() { /* ... */ } }
class Int extends Real {  Int increment() { /* ... */ } }
```

. . .

or, more accurately,

```java
class RealFun                { Real operation(Real x) { /* ... */ } }
class IntFun extends RealFun {  Int operation(Real x) { /* ... */ } }
```

* (tidbit: Sun added *covariant return types* back in Java SE 5, 2004)

## Gentzen-style subtyping deduction rules

```art
  precondition
-----------------
 postcondition
```

. . .

Example:

```art
  A true    B true
--------------------
   (A and B) true
```

## A rule for function values

```art
     Y <: X
-----------------
A -> Y <: A -> X
```

aka `->` is *covariant* with respect to its return type.

. . .

Example instance:

```art
        Int <: Real
---------------------------
Real -> Int <: Real -> Real
```
. . .

All caller can do is *get* an `X` from calling the function;

So it is safe to *narrow* and use a `Y` as the return value.

## Contravariant with respect to argument type {.left_align}

* "Have a function of type `Real -> Int`, want one like `Int -> Int`"

* Sometimes unintuitive

"Client says they will only feed integer values into the function,
so it is safe to provide something that can consume any real number."

. . .

* Not supported by languages like Java; conflicts with method overloading.

## A more general rule with contravariance

```art
Y <: X    B <: A
-----------------
A -> Y <: B -> X
```

aka `->` is covariant with respect to its return type,
and `->` is *contravariant* with respect to its argument type.

. . .

All caller can do is feed in more specific `B` (and get out more general `X`).

So it is safe to be more liberal and accept any `A` at all, and guarantee
the more specific `Y` as return value.

. . .

Aside: What about when domain = range?

```art
       ???
----------------
Y -> Y <: X -> X
```

<!-- FIXME TODO: try to add visualization of subtyping via Venn or puzzle pieces -->

# Does Rust have variance? {.center}

## Does Rust have variance: [experiment 1][vec_slice_variance]

[vec_slice_variance]: https://play.rust-lang.org/?gist=4777475b64e41e21863ed3dd35a76212

```art
          &Vec<i32>                   fn (&usize) -> &Vec<i32>
            |                          |
            |                          : ?
            v                          v[q]
  fn expect(&[i32])     fn expect_hof(fn (&usize) -> &[i32])
```

. . .

```rust
mod demo_variance_and_vec_slice {
    fn provide(m: &Vec<i32>) { expect(m); }
    fn expect(_nums: &[i32]) { unimplemented!() }
}
```

``` {.rust .compile_error .stripes}
mod demo_variance_and_vec_slice_hof {
    fn provide_hof(f: fn (&usize) -> &Vec<i32>) { expect_hof(f); }
    fn expect_hof(_f: fn (&usize) -> &[i32]) { unimplemented!(); }
}
```

```
error: mismatched types [E0308]
fn provide_hof(f: fn (&usize) -> &Vec<i32>) { expect_hof(f); }
                                                         ^
note: expected type `fn(&usize) -> &[i32]`
note:    found type `fn(&usize) -> &std::vec::Vec<i32>`
```

("hof" stands for "higher-order function")

## Does Rust have variance: [experiment 2][mut_imm_slice_variance]

[mut_imm_slice_variance]: https://play.rust-lang.org/?gist=1c7ac483ca7490058bc2b8ef22c1f712

```art
         &mut [i32]                 fn (&usize) -> &mut [i32]
            |                        |
            |                        : ?
            v                        v[q]
  fn expect(&[i32])     fn expect_hof(fn (&usize) -> &[i32])

```

. . .

```rust
mod demo_variance_and_mut_slice {
    fn provide(m: &mut [i32]) { expect(m); }
    fn expect(_nums: &[i32]) { unimplemented!() }
}
```

``` {.rust .compile_error .stripes}
mod demo_variance_and_mut_slice_hof {
    fn provide_hof(f: fn (&usize) -> &mut [i32]) { expect_hof(f); }
    fn expect_hof(_f: fn (&usize) -> &[i32]) { unimplemented!(); }
}
```

```
error: mismatched types [E0308]
fn provide_hof(f: fn (&usize) -> &mut [i32]) { expect_hof(f); }
                                                          ^
note: expected type `fn(&usize) -> &[i32]`
note:    found type `fn(&usize) -> &mut [i32]`
```


## Seems like Rust does not have (co)variance {.center}

. . .

### "seems like" ... hmm

# Misdirection {.center}

## A Story

Simon Sapin (paraphrased):

> I had some code that does not compile. I do not
> understand why, so to try to understand it, I
> narrowed it down to a particular use of `Cell`.
> But I didn't understand what was happening, so
> I tried making my tiny version of `Cell`, and the
> problem went away.

## The Sapin example

Let's try to implement `std::cell::Cell`, in user code.

```rust
struct MyCell<T> {
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

. . .

(Actually *completely* broken; reasons include compiler details
regarding aliasing info; we'll focus on another subtle one here.)

----

<!-- https://play.rust-lang.org/?gist=704f577e66e64a62bfb53bbb833b9c4e -->

```rust
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);

    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13;
                                           step2(&val, r_c1);
                                           println!("step1 value: {}", r_c1.value); }

    fn step2<'b>(r_val: &'b i32, r_c2: &MyCell<&'b i32>) { r_c2.set(r_val); }

    println!("  end value: {}", cell.value);
}
```

. . .

([DON'T PANIC][dont_panic_link]: We're going to step through this.)

[dont_panic_link]: https://play.rust-lang.org/?gist=704f577e66e64a62bfb53bbb833b9c4e

. . .

Output (some run on my machine):

```
step1 value: 13
  end value: 28672
```

. . .

(`28672`??? Bogus!) Where does this garbage come from?

##  { data-transition="fade" data-transition-speed="fast" }

``` {.rust}
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);
    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'b>(r_val: &'b i32, r_c2: &MyCell<&'b i32>) { r_c2.set(r_val); }
}
```

. . .

before `step1(&cell);`:

```art
                             (static data area)

     (Stack)          .----> X: 10
                      |
     cell = MyCell    |
            value  =--'
```

##  { data-transition="fade" data-transition-speed="fast" }

``` {.rust}
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);
    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'b>(r_val: &'b i32, r_c2: &MyCell<&'b i32>) { r_c2.set(r_val); }
}
```

before `step2(&val, r_c1);`:

```art
                             (static data area)

     (Stack)          .----> X: 10
                      |
 .-> cell = MyCell    |
 :          value  =--'
 |   ----
 |
 '-- r_c1

      val  13
```

##  { data-transition="fade" data-transition-speed="fast" }

``` {.rust}
static X: i32 = 10;

#[test]
fn test_mycell_short_lifetime() {
    let cell = MyCell::new(&X);
    step1(&cell);
    fn step1<'a>(r_c1: &MyCell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'b>(r_val: &'b i32, r_c2: &MyCell<&'b i32>) { r_c2.set(r_val); }
}
```

before `r_c2.set(r_val)`:

```art
                             (static data area)

     (Stack)          .----> X: 10
                      |
 .-> cell = MyCell    |
 :          value  =--'
 |   ----
 |
 +-- r_c1
 |
 |    val  13 <-------.
 |                    :
 :   ----             |
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
    fn step2<'b>(r_val: &'b i32, r_c2: &MyCell<&'b i32>) { r_c2.set(r_val); }
}
```

after `r_c2.set(r_val)`:

```art
                             (static data area)

     (Stack)                 X: 10

 .-> cell = MyCell
 :          value  ---.
 |   ----             :
 |                    |
 +-- r_c1             |
 |                    |
 :    val  13 <-----=-+
 |                    |
 |   ----             :
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
    fn step2<'b>(r_val: &'b i32, r_c2: &MyCell<&'b i32>) { r_c2.set(r_val); }
}
```

after `step2` returns:

```art
                             (static data area)

     (Stack)                 X: 10

 .-> cell = MyCell
 |          value  ---.
 :   ----             :
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
    fn step2<'b>(r_val: &'b i32, r_c2: &MyCell<&'b i32>) { r_c2.set(r_val); }
}
```

after `step1` returns:

```art
                             (static data area)

     (Stack)                 X: 10

     cell = MyCell
            value  ---.
                      |
                      :
                      |
                      |
      ???     <-------'
```

. . .

Either `MyCell` is broken, or the compiler is.

## Is `Cell` [also][show_cell_cfail] broken? {.center}

[show_cell_cfail]: https://play.rust-lang.org/?gist=ddc78b073a4b50147b343358138d47d7

``` {.rust .compile_error .stripes}
mod test_stdcell_short_lifetime {
    use std::cell::Cell;
    static X: i32 = 10;

    #[test]
    fn test_stdcell_short_lifetime() {
        let cell = Cell::new(&X);
        step1(&cell);
    }
    fn step1<'a>(r_c1: &Cell<&'a i32>) { let val: i32 = 13; step2(&val, r_c1); }
    fn step2<'b>(r_val: &'b i32, r_c2: &Cell<&'b i32>) { r_c2.set(r_val); }
}
```

. . .

Compiler output:

```
error: `val` does not live long enough
step2(&val, r_c1);
       ^~~
note: reference must be valid for the lifetime 'a as defined on the block at 775:39...
fn step1<'a>(r_c1: &Cell<&'a i32>) {
                                   ^
note: ...but borrowed value is only valid for the block suffix following statement 0 at 776:26
    let val: i32 = 13;
                      ^
```

## Difference between `MyCell` and `Cell`?

``` {.rust}
struct MyCell<T> {
    value: T,
}

pub struct Cell<T> {
    value: UnsafeCell<T>,
}

#[lang = "unsafe_cell"]
pub struct UnsafeCell<T: ?Sized> {
    value: T,
}
```

. . .

`MyCell` just holds a `T`, while `Cell` holds an `UnsafeCell<T>`,
which is a "language item" known specially to the compiler.

## Why accept `MyCell` and reject `Cell`? {.center}

. . .

### *Subtyping* and *variance*

# Yes, Rust has Subtyping {.center}

## Yes, [Rust has Subtyping][ref_subtyping_demo]

[ref_subtyping_demo]: https://play.rust-lang.org/?gist=b8cfc470abf30aa283e91197e9cf0a4b

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

## And the [other direction][promote_cfail_demo]?

[promote_cfail_demo]: https://play.rust-lang.org/?gist=9ed292d53082dae990111c9f897502bb

``` {.rust .compile_error }
fn promote<'a>(x: &'a i32) -> &'static i32 {
    return x;
}

#[test] fn promote_test() { let temp: i32 = 200; promote(&temp); }
```
```art
 |
 : 'static (`fn promote` promises a reference value for this long)
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

## References revisited

Insight: For *any* type `T` and any lifetime `'a`, clearly `&'static T`
should be valid anywhere that `&'a T` is.

. . .

```art
         &'static [i32]
            |
            |
            v
  fn expect(&[i32])
```

. . .

Gentzen style

```art
 'static outlives 'a
---------------------
 &'static T <: &'a T
```

## A more general rule

For any type `T` and lifetimes `'a`, `'b`, if `'b` outlives `'a`, then
`&'b T` should be valid anywhere `&'a T` is.

```art
    'b outlives 'a
---------------------
    &'b T <: &'a T
```

## Further generalization

Already established we should have `&'static T <: &'a T`.

. . .

What about a reference *to* a reference?

Should `&'a &'static T <: &'a &'a T` ...?

. . .

Intuition: All you can do with a `&X` is *read* data from the `X` it holds.

Analogous to a function `A -> Y <: A -> X`

. . .

```art
      'b outlives 'a
           Y <: X
  -----------------------------
       &'b Y <: &'a X
```

. . .

`&X` is *covariant* with respect to `X`

. . .

But that's `&X`; what about `&mut X`?

## What about mut references

[mut_refs_cfail_demo]: https://play.rust-lang.org/?gist=f93578cc4f20298616f8badf68aac500

Should `&'a mut &'static T <: &'a mut &'a T` ...?

. . .

Allowing that [exposes][mut_refs_cfail_demo]:


``` {.rust .compile_error}
mod promote_short_to_static {
    static X: i32 = 10;
    fn test() {
        let mut ptr: &'static i32 = &mut X;
        step1(&mut ptr);
    }
    fn step1<'a>(r1: &'a mut &'static i32) { let val: i32 = 13; step2(r1, &val); }
    fn step2<'b, T>(r2: &'b mut &'b T, r_val: &'b T) { *r2 = r_val; }
}
```

. . .

```art
                             (static data area)

     (Stack)          .----> X: 10
                      :
 .-> ptr  ------------'
 :
 |   ----
 |
 +-- r1
 |
 :    val  13 <-----=-.
 |                    |
 |   ----             :
 |                    |
 |  r_val ------------'
 |
 '-- r2
```

## Invariance!

Insight: for any type `X` and any lifetime `'a`, `&'static mut X` is valid anywhere
`&'a mut X` is.

*But* we cannot generalize to `Y <: X`

. . .

Intuition: Once you allow mutation through a reference, the type itself
must remain fixed.

## Other languages

Java example: Does this compile? (FYI `Float <: Number`)

```java
static void modify_array(Number[] numArray) { numArray[0] = new Float(3.14); }
```

. . .

Java array `T[]` is *covariant* with respect to `T`.

. . .

What happens here?

<!-- FIXME consider adding *caution* signal orange CSS here. -->

```java
static void mut_array() {
    Integer[] intArray = new Integer[1]; modify_array(intArray); }
```

. . .

```
Exception in thread "main" java.lang.ArrayStoreException: java.lang.Float
    at Examples.modify_array(Examples.java:25)
    at Examples.mut_array(Examples.java:21)
    at Examples.main(Examples.java:9)
```

. . .

(Old ~~wart~~ historical artifact; predates Java support for generics.)

## Are Rust function types variant?

```art
          &'static i32              fn (&usize) -> &'static i32
            |                        |
            |                        : ?
            v                        v[q]
  fn expect(&i32)     fn expect_hof(fn (&usize) -> &i32)
```

. . .

```rust
mod demo_variance_and_static_ref {
    fn provide(m: &'static i32) { let val = 13; expect(&val, m); }
    fn expect<'a>(_: &'a i32, _r: &'a i32) { unimplemented!() }
}

mod demo_variance_and_static_ref_hof {
    fn prov_hof(f: fn(&usize) -> &'static i32) { let val = 13; exp_hof(&val, f); }
    fn exp_hof<'a>(_: &'a i32, _f: fn (&'a usize) -> &'a i32) { unimplemented!() }
}
```

. . .

[Compiles][variance_and_static_ref_demo]!

[variance_and_static_ref_demo]: https://play.rust-lang.org/?gist=52bc0ea4b6d2b5fad2781aeccaf18e38

. . .

Functions even contravariant with respect to their argument types

(Rust does not have method overloading so no conflict there)


# Behind the curtain: variance {.center}

## Where does variance come from?

Compiler deduces the variance of a type (with respect to its type
parameters) based on the structure of that type.

```rust
struct OuterInline<T> { one: T, inner: InnerInline<T> }
struct InnerInline<T> { data: T }
```

`InnerInline` and `OuterInline` both covariant with respect to `T`

. . .

```rust
struct OuterRef<'a, T: 'a> { one: &'a mut T, inner: InnerRef<'a, T> }
struct InnerRef<'a, T: 'a> { data: &'a T }
```

`InnerRef` is covariant w.r.t. `T`, while
`OuterRef` is *invariant* w.r.t. `T`

. . .

If compiler sees a `PhantomData<SomeType>`, it traverses the
structure of `SomeType` as if it were embedded directly.

## What's up with `Cell`, `UnsafeCell`

`UnsafeCell<T>` is *invariant* with respect to `T`,
(and that bubbles out to `Cell<T>`).

## What's up with `MyCell`

Structural definition of `MyCell` alone
implies it is *covariant* w.r.t. `T`

This (broken) method violates rules associated with covariance:

``` {.rust}
    fn set(&self, value: T) {
        use std::ptr;
        unsafe {
            ptr::write(&self.value as *const _ as *mut _, value);
        }
    }
```

. . .

Must impose *invariance*

. . .

Use `PhantomData<fn (T) -> T>` in `MyCell<T>`: one way

Use `UnsafeCell<T>` in `MyCell<T>`: better way

# Behind the curtain: "duck subtyping" {.center}

## Examples from beginning {.center}

The earlier examples are not kind of subtyping that references have.

![](rabbit_hat_wand.svg)

## What is actually happening? {.center}

## Coercions, Auto-Borrowing, Protocols {.center}

# Coercions {.center}

## Receiver coercion

For receiver for a method (i.e. the `x` in `x.m(...)`):

Compiler will automatically insert a borrow or dereferences
to find a type that provides method `m`.

See also [StackOverflow Q](http://stackoverflow.com/questions/28519997/what-are-rusts-exact-auto-dereferencing-rules/28552082#28552082)

. . .

That, combined with dereference of a `Copy` type, explains
``` {.rust}
    let mut a = ['a', '1']; let b = &['b', '4']; let c = &mut ['c', '7'];
    // [char; 2]            &[char; 2]           &mut [char; 2]
    a.by_val();             b.by_val();          c.by_val();
    a.by_ref();             b.by_ref();          c.by_ref();
    a.by_mut();             /*  ...  */          c.by_mut();
    println!("interesting: (a,b,c): {:?}", (a,b,c));
```

. . .

(Note: Parameters other than receiver do not get this special treatment)

<!-- FIXME TODO: WHY -->

## Deref coercion

When you have a type `&Y`, where `Y` can be dereferenced to yield `X`,
then compiler will automatically coerce `&Y` to `&X` by inserting
necessary derefers.

See [RFC 271: "Deref Conversions"][RFC 271]

[RFC 271]: https://github.com/rust-lang/rfcs/blob/master/text/0241-deref-conversions.md

. . .

That, combined with `impl` such that `Vec<T>: Deref<Target=[T]>`, explains

```art
        &Vec<i32>
            |
            v
  fn expect(&[i32])
```

<!-- Out of time

## Auto Borrowing and Reborrowing

(imm and mut)


```art
         &mut [i32]
            |
            v
  fn expect(&[i32])
```

-->

## Auto reborrows

As if things weren't confusing enough ...

If you pass a mutable `value` reference into an expression context that is
itself inferred to be expecting a reference, the compiler will automatically
insert `&mut *value`.

. . .

Main effect of this: passing a `&mut T` does not always *move* it;
sometimes it implicitly reborrows (so that you can mutate the `&mut`
after the call, rather than losing access to it).

. . .

See [bluss "Stuff the Identity Function Does"](https://bluss.github.io/rust/fun/2015/10/11/stuff-the-identity-function-does/)

## Protocols

How did we get `EndToEndErr` from the results of `phase_1`/`phase_2`?

. . .

``` {.rust}
fn phase_1(x: Ast) -> Result<Mir, PhaseOneErr> { /* ... */ }
fn phase_2(y: Mir) -> Result<Out, PhaseTwoErr> { /* ... */ }

fn composed(a: Ast) -> Result<Out, EndToEndErr> {
    let mir = try!(phase_1(a));
    let out = try!(phase_2(mir));
    return Ok(out);
}
```

. . .

Are `PhaseOneErr` and `PhaseTwoErr` subtypes of `EndToEndErr`?

. . .

Answer: Magic is hidden behind the `try!` (and some trait impls)

----

Answer: Magic is hidden behind the `try!` (and some trait impls)

. . .

``` {.rust}
    let mir = try!(phase_1(a));
```

expands to:

``` {.rust}
    let mir =
        match phase_1(a) {
            ::std::result::Result::Ok(val) => val,
            ::std::result::Result::Err(err) => {
                return ::std::result::Result::Err(::std::convert::From::from(err))
            }
        };
```

. . .

(New `?` form today is same as `try!`; easier to
show `try!` expansion)

. . .

inserts conversion via `std::convert::From`
transforming specific error to whatever error is expected
in context of expression

----

``` {.rust}
            ::std::result::Result::Err(err) => {
                return ::std::result::Result::Err(::std::convert::From::from(err))
            }
```

inserts conversion via `std::convert::From`
transforming specific error to whatever error is expected
in context of expression

. . .

```rust
enum EndToEndErr {
    P1(PhaseOneErr),
    P2(PhaseTwoErr),
}

impl From<PhaseOneErr> for EndToEndErr {
    fn from(phase_1_err: PhaseOneErr) -> EndToEndErr {
        EndToEndErr::P1(phase_1_err)
    }
}

impl From<PhaseTwoErr> for EndToEndErr {
    fn from(phase_2_err: PhaseTwoErr) -> EndToEndErr {
        EndToEndErr::P2(phase_2_err)
    }
}
```

. . .

(So, all the magic here is in macros and trait system.)

## Gotchas re: coercions and auto-(re)borrows

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

# Conclusion  {.center}

## Pop Quiz {.separated}

---------------------     -------- ------------------
`&'static mut T`           `<:` ?         `&'a mut T`

`&'a &'static mut T`       `<:` ?     `&'a &'a mut T`

`&'a mut &'static T`       `<:` ?     `&'a mut &'a T`
---------------------     -------- ------------------

. . .

One answer: How often do you *actually* answer such questions?

(be aware of subtyping, but need not be foremost in your mind)

. . .

In case you care: Yes, Yes, No

## Why does variance matter?

Getting covariance vs invariance right matters for soundness

. . .

But you should not need to think about it at all if you are not writing
`unsafe { ... }`

(it is solely compiler's job until `unsafe` gets involved)

## More Info

See RFCs

The Book

and the Rustonomicon

And contribute back to them!

## End Thoughts {.center}

* Keep asking questions

* "Show me the code!"

* Do the experiments

* Magic: As real as you want it to be

## Thanks! {.center}
