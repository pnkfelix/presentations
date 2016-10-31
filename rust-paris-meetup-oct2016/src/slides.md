## Subtyping in Rust

### Felix Klock (`@pnkfelix`), Mozilla Research
### Rust Paris Meetup; 31 Oct 2016

<!-- Slides: [`http://bit.ly/2cMFbYB`](http://bit.ly/2cMFbYB) -->

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

# Disclaimer: Rehashing Rust Fest Talk {.center}

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

## Exploring method calls

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

## Why does representation compatibility matter?

Assume need one representation for type `A` and another for ("compatible") type `B`.
(Say, three words for `A` and two words for `B`...)

What happens holding a `Vec<A>` and a method wants a `Vec<B>`?

. . .

```art
+-------+        +-------+
| .---. |        | .---. |
| |a_0| |        | |b_0| |
| |   | |        | |   | |
| |   | |        | '---' |
| '---' |        | .---. |
| .---. |        | |b_1| |
| |a_1| |   -->  | |   | |
| |   | |        | '---' |
| |   | |        | .---. |
| '---' |        | |b_2| |
| .---. |        | |   | |
| |a_2| |        | '---' |
| |   | |        |       |
| |   | |        |       |
| '---' |        |       |
|       |        |       |
|       |        :       |
+-------+        +-------+
```

. . .

Rewrite the `Vec<A>` contents in order to pass it as a `Vec<B>`?

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

# Does Rust have variance? {.center}

## Does Rust have variance: [experiment][vec_slice_variance]

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


## Seems like Rust does not have (co)variance {.center}

(other examples, like passing `&mut T` as a `&T`, fail to generalize
in function variant positions.)

. . .

### "seems like" ... hmm

# Yes, Rust has Subtyping {.center}

## Yes, [Rust has Subtyping][ref_subtyping_demo]

But I haven't shown you any example of it yet.

## Subtyping from references w/ nested *lifetimes*

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
(Sound, because `'static` outlives `'a`)

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

. . .

### *Crucial* infrastructure for Rust's usability

Can often use single lifetime param in lifetime-polymorphic functions,
because `rustc` allows passing in longer lifetimes when appropriate
(via subtyping).

. . .

Difference between

``` {.rust}
fn ugly<'base, 's1: 'base, 's2: 'base>(a: &'s1 A, b: &'s2 B) -> &'base C { ... }
```
and
``` {.rust}
fn better<'lone>(a: &'lone A, b: &'lone B) -> &'lone C { ... }
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

Allowing that [exposes][mut_refs_cfail_demo] (explained in detail in Rust Fest talk):


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
     (Stack)                 (static data area)

 .-> ptr  ------------.
 |                    |[bad]
 |   ----             |      X: 10
 |                    |
 +-- r1               :
 |            <-+-----'
 |    val  13
 |            <---.
 |   ----         |
 |                |
 |  r_val --------'
 |
 '-- r2

[bad]: stroke="red"
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

# Behind the curtain: "duck subtyping" {.center}

## Examples from beginning {.center}

## Coercions, Auto-Borrowing, Protocols {.center}

# Coercions {.center}

## Receiver coercion

For receiver for a method (i.e. the `x` in `x.m(...)`):

Compiler will automatically insert a borrow or dereferences
to find a type that provides method `m`.

See also [StackOverflow Q](http://stackoverflow.com/questions/28519997/what-are-rusts-exact-auto-dereferencing-rules/28552082#28552082)

. . .

(Note: Parameters other than receiver do not get this special treatment)

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

## More Info

RFCs

The Book

and the Rustonomicon

And contribute back to them!

. . .

Also: See Rust Fest Video (when its posted...)

## Thanks! {.center}
