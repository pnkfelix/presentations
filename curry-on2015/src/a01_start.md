# Rust: What? Why? (How?) { .center .center_align }

## Rust: What { .left_align }

* New systems programming language

    * fast; FFI interface; data layout control

    * compete (and interface with) with C/C++

. . .

* Mix in the classic hits of PL

    * user-defined iterators, RAII, objects with vtable method dispatch, generics / F-bounded polymorphism, algebraic data types, affine types, et cetera

. . .

* Safety

    * Memory-safe, data-race free

    * <a href="http://blog.rust-lang.org/2015/04/10/Fearless-Concurrency.html">Fearless concurrency</a>

## Rust: Where {.left_align .center}

Language and API docs

  * All linked from top of [http://www.rust-lang.org/]

  * Starting points

    The Book: [https://doc.rust-lang.org/stable/book/]

    Standard API: [https://doc.rust-lang.org/stable/std/]

  * Playpen: [http://play.rust-lang.org/]


[http://www.rust-lang.org/]: http://www.rust-lang.org/

[https://doc.rust-lang.org/stable/book/]: https://doc.rust-lang.org/stable/book/

[https://doc.rust-lang.org/stable/std/]: https://doc.rust-lang.org/stable/std/


[http://play.rust-lang.org/]: http://play.rust-lang.org/

## Playpen (encore) {.center .center_align}

[http://play.rust-lang.org/]

# Motivational Demos {.center}

## A taste

  * Three fast *"amuse bouches"*
    * not the main course
    * (not even an appetizer)

## Abstraction without overhead { data-transition="fade-out" }

The below [loop demo] compiles down to tight code:

<!--
```rust
#[allow(dead_code)]
fn main1() {
    let v1: Vec<i32> = (-100..10).collect();
    let s1 = sum_pos(&v1);
    let v2: Vec<i32> = (-100..1000).collect();
    let s2 = sum_pos(&v2);
    println!("v1.len: {} s1: {} v2.len: {} s2: {}", v1.len(), s1, v2.len(), s2);
}

#[allow(dead_code)]
#[inline(never)]
```
-->

```rust
// sums all the positive values in `v`
fn sum_pos(v: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in v.iter().filter(|i| **i > 0) {
        sum += *i;
    }
    sum
}
```

[loop demo]: http://is.gd/weGnJ0
<!-- https://play.rust-lang.org/?gist=23a69161dd4421e2925f -->

## Abstraction without overhead { data-transition="fade" }

Generated x86_64 machine code for `fn sum_pos`{.rust}:

```nasm
	leaq	(%rdi,%rsi,4), %rcx
	xorl	%eax, %eax
	jmp	.LBB5_1
.LBB5_3:
	addl	%edx, %eax
	.align	16, 0x90
.LBB5_1:
	cmpq	%rdi, %rcx
	je	.LBB5_4
	movl	(%rdi), %edx
	addq	$4, %rdi
	testl	%edx, %edx
	jle	.LBB5_1
	jmp	.LBB5_3
.LBB5_4:
	retq
```

## Abstraction without overhead { data-transition="fade-in slide-out" }

Or, if the first was not high-level enough:

``` {.rust}
fn foo(v: &Vec<i32>) -> i32 {
    v.iter().filter(|i| **i > 0).map(|i| *i).sum()
}
```

(This [second loop demo] compiles down to the *same* tight code!)

[second loop demo]: http://is.gd/RPVIjt

## Memory safety

Example: catches [iterator invalidation bugs](http://is.gd/ShihgA)

``` {.rust .compile_error}
fn this_wont_compile(v: &mut Vec<i32>) -> i32 {
    let mut sum = 0;
    for &i in v.iter() {
        sum += i;
        if i > 0 { v.push(0); }
        //         ~~~~~~~~~ invalid! (might realloc
        //                   the backing storage for `v`)
    }
    sum
}
```

``` {.fragment}
error: cannot borrow `*v` as mutable because it is also borrowed
       as immutable
        if i > 0 { v.push(0); }
                   ^
note: previous borrow of `*v` occurs here; the immutable borrow
      prevents subsequent moves or mutable borrows of `*v` until
      the borrow ends
    for &i in v.iter() {
              ^
```

## Slick, Fearless Concurrency

See also [Fearless Concurrency] blog post.

[Fearless Concurrency]: http://blog.rust-lang.org/2015/04/10/Fearless-Concurrency.html

<!--
```rust
fn seq_max(partial_data: &[u8]) -> u8 {
    *partial_data.iter().max().unwrap()
}
```
-->

```rust
use std::thread;
fn par_max(data: &[u8]) -> u8 {
    if data.len() <= 4 { return seq_max(data); }
    let len_4 = data.len() / 4; // DATA = [A .., B .., C .., D..]
    let (q1, rest) = data.split_at(len_4);    // (A.. \ B..C..D..)
    let (q2, rest) = rest.split_at(len_4);    //  (B.. \ C..D..)
    let (q3, q4)   = rest.split_at(len_4);    //   (C.. \ D..)
    let t1 = thread::scoped(|| seq_max(q1));  // fork A..
    let t2 = thread::scoped(|| seq_max(q2));  // fork B..
    let t3 = thread::scoped(|| seq_max(q3));  // fork C..
    let v4 = seq_max(q4);                     // compute D..
    let (v1, v2, v3) = (t1.join(), t2.join(), t3.join()); // join!
    return seq_max(&[v1, v2, v3, v4]);
}
```

(caveat: [above](http://is.gd/zOH5aL) is using unstable `thread::scoped` API)

<!--
```rust
#[test]
fn hi() {
    let mut v = vec![2, 1, 10, 9, 8, 7, 6, 5, 4, 3];
    let m = par_max(&v);
    assert_eq!(m, 10);
}
```
-->

# Rust: Why?  { .center }

## Rust: Why?  { .left_align .center }

* Why is Mozilla investing in this?

. . .

* To compete

    C/C++ impedes ability to compete in the browser market

. . .

* Fast experimentation (and deployment)

    ⇒ Competitive Advantage

## Servo { .center .left_align }

. . .

  browser implementation research platform

. . .

  written in Rust

. . .

### Experiments

  * parallel paint

  * parallel layout

  * parallel css selector matching

## Rust: A note on goals

<div class="fragment">
What is "soundness"?
</div>

<div class="fragment">
Segfault is not okay
</div>

<div class="fragment">

But: "clean" shutdown (e.g. on internal error) *is* okay

</div>

<div class="fragment">
Rust has `panic!`{.rust}, and atop it `assert!`{.rust} etc

(A panic unwinds the stack, cleaning up resources)

### So

</div>

Rust uses dynamic checks (and resorts to `panic!`{.rust})
where appropriate

<div class="fragment">

### But

Prefers static assurances *when feasible*

</div>

# Rust: How? { .center_align .center }

## Rust: How? (The Big Ideas) { .center_align .center }

### Ownership + Move Semantics

<div class="fragment">
(explicit resource control)
</div>

### Borrowing

<div class="fragment">
(brings back reference semantics)
</div>

### Lifetimes

<div class="fragment">
(encode safety constraints between references)
</div>

## [Move Semantics](http://is.gd/IZyTXN)

Create (and modify) owned:
```rust
#[test]
pub fn create_owned() {
    let mut vec = Vec::new();         //  + (`vec` created ...
    vec.push(2000);                   //  |  ... and
    vec.push( 400);                   //  |       also
    vec.push(  60);                   //  |        mutated ...
    println!("vec: {:?}", vec);       //  |
    let the_sum = sum(vec);           //  o ... and moved)
    // (starting here, access to `vec` is static error)
    println!("the_sum: {}", the_sum);
}
```

At scope end, clean up initialized (+ unmoved) variables

```rust
fn sum(v: Vec<i32>) -> i32 {          //  o
   let mut accum = 0;                 //  |
   for i in v.iter() { accum += *i; } //  |
   accum // (p.s. where is `return`?) //  |
}                                     //  * (`v` destroyed/freed)
```

## Freely Copied Values versus Moved Objects

Integers, characters, etc use copy semantics

``` {.rust}
let x: i32 = calculate();
let y = x;
// can still use `x` here
```

Structured data uses move semantics by default

 * Compiler checks attempts to opt-in to `Copy` "interface"
     * ("interface", aka "trait bound" in Rust)

## [Moved data errors](http://is.gd/aYKrYd)

``` {.rust .compile_error}
#[test]
fn demo_owned_vs_copied() {
    let moving_value = vec![1, 2, 3];
    let copied_value = 17;
    let tuple = (moving_value, copied_value);

    println!("copied_value: {:?}", copied_value);
    println!("moving_value: {:?}", moving_value);
}
```

``` {.error_message .fragment}
error: use of moved value: `moving_value` [E0382]
    println!("moving_value: {:?}", moving_value);
                                   ^~~~~~~~~~~~

note: `moving_value` moved here because it has type
      `collections::vec::Vec<i32>`, which is non-copyable
    let tuple = (moving_value, copied_value);
                 ^~~~~~~~~~~~
```


## [Moves insufficient on their own](http://is.gd/qwc0iq)

Imagine programming without reuse

``` { .rust .compile_error }
#[test]
fn moves_insufficient() {
    let vec = expensive_vector_computation();

    let result1 = some_vec_calculation(vec); // <-- `vec` moved here

    let result2 = other_calculation(vec); // oops, `vec` is *gone*

    combine(result1, result2);

}
```
<!--
```rust
fn expensive_vector_computation() -> Vec<i32> { vec![] }
fn some_vec_calculation<T>(t: T) { }
fn other_calculation<T>(t: T) { }
fn combine(x: (), y: ()) { }
```
-->

``` {.fragment .error_message}
error: use of moved value: `vec` [E0382]
    let result2 = other_calculation(vec); // oops
                                    ^~~
note: `vec` moved here because it has type
      `collections::vec::Vec<i32>`, which is non-copyable
    let result1 = some_vec_calculation(vec); // <-- `vec` moved here
                                       ^~~
```

# Learning to Share {.center}

## [Borrowing](http://is.gd/ez9JrK) {.center}

*Lend* references to values

```rust
#[test]
fn hooray_for_borrows() {
    let vec = expensive_vector_computation();

    let result1 = some_vec_calculation(&vec); // <-- lend out `vec`

    let result2 = other_calculation(&vec); // <-- lend again, no prob

    combine(result1, result2);

} // (`vec` is destroyed/freed aka "dropped" here)
```

``` {.fragment}
                                    &vec
                                    ~~~~
                                      |
                              a borrow expression
```

## Mo' features, mo' problems {.center}

 * Why are safety violations generally hard to detect?

   * It is due to *aliasing*

   * Borrows *reintroduce* aliasing

### Q: How to ensure safety in presence of aliasing?  {.fragment}

### A: Restrict the aliasing {.fragment}

## Restricted aliasing {.center}

Analogy: RW-lock

  * Many readers at once, *or* one writer with exclusive access

  * Read-only operations do not require exclusive access

  * Exclusive access requires there are no other readers

Rust uses similar model (but at compile-time) for borrows

## The Family of Types

 * `T`: base type. Moves, unless bounded by `Copy` trait

 * `&T`: shared ref, "read-only" access; copyable

    * programmer (+ compiler) must assumed aliased

    * (i.e. "many readers")

 * `&mut T`: "mutable" ref, exclusive access; non-copy

    * assured unaliased

    * (i.e. "at most one writer")

# Concrete Examples of Types {.center}

## [readin', writin', and transfer'n](http://is.gd/QRmpaH)

```rust
fn read(v: &Vec<String>) -> String {
    let first: &String = &v[0]; // borrow ref to first elem
    println!("v[0]: {}", first);
    return first.clone();
}
```

. . .

<!-- TODO: show in future slide how returning &str breaks down) -->

```rust
fn modify(v: &mut Vec<String>, name: &str) {
    let freshly_created = format!("Hello {}", name);
    v.push(freshly_created);
}
```

. . .

```rust
fn consume(v: Vec<String>) -> String {
    for s in v { return s; }
    panic!("v was empty?!?");
}
```

## [Calls to the three variants](http://is.gd/QRmpaH)

```{.rust}
fn read(v: &Vec<String>) -> String { ... }
fn modify(v: &mut Vec<String>, name: &str) { ... }
fn consume(v: Vec<String>) -> String { ... }
```

```rust
fn user_input() -> String { format!("World") }

#[test]
fn demo() {
    let mut v: Vec<String> = vec![user_input()];
    let first = read(&v).clone();
    modify(&mut v, &first);
    consume(v);
}
```


# More on Scopes {.center}

## [Borrowing (immutably)](http://is.gd/oeTVPs) { data-transition="fade-out" }

```rust
#[test]
fn show_some_borrows() {

    let v1 = vec![1, 2, 3];
    let v2 = vec![4, 5, 6];

    let r1 = &v1;
    let r2 = &v2;
    foo(r1);
    foo(r2);

}
```
<!-- -->
```rust
fn foo<'a>(v: &'a Vec<i32>) { println!("v[1]: {}", v[1]); }
```

`&v1` and `&v2` are *borrowing* `v1` and `v2`.

## [Scopes and Lifetimes](http://is.gd/7uHned) { data-transition="fade-in" }

```rust
#[test]
fn show_some_lifetimes() {

    let v1 = vec![1, 2, 3]; //                 +
    let v2 = vec![4, 5, 6]; //            +    |
                            //            |    |
    let r1 = &v1;           //       +    |    |
    let r2 = &v2;           //  +    |    |    |
    foo(r1);                //  |    |    |    |
    foo(r2);                // 'r2  'r1  'v2  'v1
                            //  |    |    |    |
}                           //  *    *    *    *
```

``` {.rust}
fn foo<'a>(v: &'a Vec<i32>) { println!("v[1]: {}", v[1]); }
```

Each borrow selects "appropriate" lifetime `'a`.

## [Borrow Checking Prevents Errors](http://is.gd/bRKjWu) { data-transition="fade-out" }

``` {.rust .compile_error}
fn borrow_checking_prevents_errors() {

    let v1 = vec![1, 2, 3];      //        +
                                 //        |
    let r1 = &v1;                //  +    'v1
                                 //  |     |
    consume(v1);                 // 'r1   (moved)
    foo(r1);                     //  |
}                                //  *
```

```{.rust}
    fn foo<'a>(v: &'a Vec<i32>) { println!("v[1]: {}", v[1]); }
    fn consume(v: Vec<i32>) { /* `v` *dropped* at scope end */ }
```

`foo(r1)` attempts an indirect read of `v1`

``` {.fragment}
error: cannot move out of `v1` because it is borrowed
    consume(v1);
            ^~
note: borrow of `v1` occurs here
    let r1 = &v1;
              ^~
```

## [Lifetimes and Lexical Scope](http://is.gd/IA6Vlj) { data-transition="fade-in" }

``` {.rust .compile_error}
fn borrow_checking_may_seem_simple_minded() {

    let v1 = vec![1, 2, 3];      //        +
                                 //        |
    let r1 = &v1;                //  +    'v1
                                 //  |     |
    consume(v1);                 // 'r1   (moved)
    // (no call to read)         //  |
}                                //  *
```

```{.rust}
    fn foo<'a>(v: &'a Vec<i32>) { println!("v[1]: {}", v[1]); }
    fn consume(v: Vec<i32>) { }
```

```
error: cannot move out of `v1` because it is borrowed
    consume(v1);
            ^~
note: borrow of `v1` occurs here
    let r1 = &v1;
              ^~
```

(artifact of lexical-scope based implementation)

## [Built on lexical scopes, but non-trivial](http://is.gd/aJR3jP) { data-transition="slide-in fade-out" }

```rust
#[test]
fn copying_can_extend_a_borrows_lifetime_1() {
    fn foo<'a>(v: &'a Vec<i32>) { println!("v[1]: {}", v[1]); }
    let v1 = vec![1, 2, 3]; //         +
    let v2 = vec![4, 5, 6]; //         |    +
    let r2 = {              //         |    |
        let r1 = &v1;       //  +      |    |
        //       ~~~ <--- A //  |      |    |
        foo(r1);            // 'r1     |    |
        &v2                 //  |     'v1  'v2
    };                      //  *  +   |    |
    // (maybe mutate `v1`   //     |   |    |
    // here someday?)       //     |   |    |
                            //    'r2  |    |
    foo(r2);                //     |   |    |
}                           //     *   *    *
```

How long should the borrow `&v1` last?

<div class="notes">
In this case, the borrow marked "A" only needs to last
long enough for the call to `foo(r1)`; after that point,
the borrow is never needed.
</div>

## [Built on lexical scopes, but non-trivial](http://is.gd/Kse2Gz) { data-transition="fade-in slide-out" }

```rust
#[test]
fn copying_can_extend_a_borrows_lifetime_2() {
    fn foo<'a>(v: &'a Vec<i32>) { println!("v[1]: {}", v[1]); }
    let v1 = vec![1, 2, 3]; //         +
    let v2 = vec![4, 5, 6]; //         |    +
    let r2 = {              //         |    |
        let r1 = &v1;       //  +      |    |
        //       ~~~ <--- A //  |      |    |
        foo(r1);            // 'r1     |    |
        r1  // <--------- B //  |     'v1  'v2
    };                      //  *  +   |    |
    // (maybe mutate `v1`   //     |   |    |
    // here someday?)       //     |   |    |
                            //    'r2  |    |
    foo(r2);                //     |   |    |
}                           //     *   *    *
```

How long should the borrow `&v1` last now?

<div class="notes">
In this case, the borrow marked "A" needs to last longer!
The value of `r1` is being copied into `r2` on the
line marked "B", so the borrow marked "A" needs to
include the scope of both `'r1` and `'r2`.
</div>

## imm-borrows: can be copied freely { data-transition="fade-out" }

(super super useful to be able to share readable data!)

## [imm-borrows: can be copied freely](http://is.gd/MioaBw) { data-transition="fade-in fade-out" }

Implications:

  * must assume aliased (perhaps by another thread)
  * therefore *not safe* to mutate in general

``` {.rust .compile_error}
#[test]
fn demo_cannot_mutate_imm_borrow() {
    let mut v1 = vec![1, 2, 3];
    let b = &v1;
    let (b1, b2, b3) = (b, b, b);
    try_modify(b);
    println!("v1: {:?}", v1);
}

fn try_modify(v: &Vec<i32>) {
    v.push(4);
}
```

```{ .fragment .error_message }
error: cannot borrow immutable borrowed content `*v` as mutable
    v.push(4);
    ^
```

## [imm-borrows: can be copied freely](http://is.gd/MioaBw) { data-transition="fade-in slide-out" }

Implications:

  * must assume aliased (perhaps by another thread)
  * therefore *not safe* to mutate in general

``` {.rust .compile_error}
#[test]
fn demo_cannot_mutate_imm_borrow() {
    let mut v1 = vec![1, 2, 3];
    let b = &v1;
    let (b1, b2, b3) = (b, b, b);
    try_modify(b);
    println!("v1: {:?}", v1);
}

fn try_modify(v: &Vec<i32>) {
    v.push(4);
}
```

```
WHAT
      A
         BUMMER!!!
```

##  my precious imperative algorithms! {.center}


## [`&mut`{.rust} borrows](http://is.gd/FWAPCw) {.center}


```rust
#[test]
fn demo_can_mutate_mut_borrow() {
    let mut v1 = vec![1, 2, 3];
    modify_vec(&mut v1);
    println!("v1: {:?}", v1);
}

fn modify_vec(v: &mut Vec<i32>) {
    v.push(4);
}
```

```{ .fragment }
v1: [1, 2, 3, 4]
```

## What does `&mut`{.rust} mean (crucial) { .center data-transition="zoom-out" }

For many (but not all) types, safe mutation *requires* exclusive access

Any operation requiring exclusive access should either:

  * take ownership, or,

  * take an `&mut`{.rust}-reference

## [Exclusive Access versus Ownership](http://is.gd/J5YMKs)

```rust
fn take_by_value(v: Vec<i32>) { let mut v = v; v.push(4);  }
fn take_mut_borrow(b: &mut Vec<i32>) { b.push(10); }
// seemingly similar in power
```


``` {.rust .compile_error}
#[test]
fn demo_exclusive_access_versus_ownership() {
    let (mut v1, mut v2) = (vec![1, 2, 3], vec![7, 8, 9]);
    take_by_value(v1);
    take_mut_borrow(&mut v2);
    println!("v1: {:?} v2: {:?}", v1, v2);
}
```

``` { .fragment .error_message }
error: use of moved value: `v1` [E0382]
    println!("v1: {:?} v2: {:?}", v1, v2);
                                  ^~
note: `v1` moved here
    take_by_value(v1);
                  ^~
```

. . .

ownership ⇒ power + responsibility (for dropping)

`&mut` ⇒ power without responsibility; (can only *swap*)


# `&mut`{.rust} safety enforcement {.center}

## [Data has at most one `&mut`{.rust} borrow](http://is.gd/Z72hwM) { data-transition="slide-in fade-out" }

```rust
fn take2<'a>(v1: &'a mut Vec<i32>, v2: &'a Vec<i32>) { }
```

``` { .rust .compile_error }
#[test]
fn demo_cannot_mut_borrow_multiple_times() {
    let mut v1 = vec![1, 2, 3];
    let mut v2 = vec![1, 2, 3];
    take2(&mut v1, &mut v2); // <-- this is okay
    take2(&mut v1, &mut v1);
}
```

``` { .fragment .error_message }
error: cannot borrow `v1` as mutable more than once at a time
    take2(&mut v1, &mut v1);
                        ^~
note: previous borrow of `v1` occurs here; the mutable borrow
      prevents subsequent moves, borrows, or modification of
      `v1` until the borrow ends
    take2(&mut v1, &mut v1);
               ^~


```

## [Cannot alias `&mut`{.rust}-borrowed data](http://is.gd/MRxBeK) { data-transition="fade" }

```{.rust}
fn take2<'a>(v1: &'a mut Vec<i32>, v2: &'a Vec<i32>) { }
```

``` {.rust .compile_error }
#[test]
fn demo_cannot_alias_mut_borrowed_data() {
    let mut v1 = vec![1, 2, 3];
    let mut v2 = vec![1, 2, 3];
    take2(&mut v1, &v2); // <-- this is okay
    take2(&mut v1, &v1);
}
```

``` { .fragment .error_message }
error: cannot borrow `v1` as immutable because it is also borrowed
       as mutable
    take2(&mut v1, &v1);
                    ^~
note: previous borrow of `v1` occurs here; the mutable borrow 
      prevents subsequent moves, borrows, or modification of `v1`
      until the borrow ends
    take2(&mut v1, &v1);
               ^~
```

## [`&mut T`{.rust} is non-copy](http://is.gd/vBPs15) { data-transition="fade-in slide-out" }

```{.rust}
fn take2<'a>(v1: &'a mut Vec<i32>, v2: &'a Vec<i32>) { }
```

``` {.rust .compile_error }
#[test]
fn demo_cannot_copy_mut_borrows() {
    let mut v1 = vec![1, 2, 3];
    let b = &mut v1;
    let c = b;
    take2(b, c);
}
```

``` { .fragment .error_message }
error: use of moved value: `*b` [E0382]
    take2(b, c);
          ^
note: `b` moved here because it has type
      `&mut collections::vec::Vec<i32>`, which is moved by default
    let c = b;
        ^
```

(ensures exclusive access)

# Wither `a.method()`{.rust} ? {.center}

## [Rust has methods too](http://is.gd/2r7DRl)

```rust
struct Point { x: i32, y: i32 }

impl Point {
    fn distance_from_origin(&self) -> i32 {
        let Point { x, y } = *self;
        let sum = (x*x + y*y) as f64;
        sum.sqrt() as i32
    }
}

#[test]
fn demo_dist() {
    let p = Point { x: 3, y: 4 };
    assert_eq!(5, p.distance_from_origin());
}
```

## Method signatures { .center }

* `self`{.rust}: *consumes* receiver
* `&self`{.rust}: *accesses* receiver
* `&mut self`{.rust}: *mutates* receiver

## Method signatures: &self

Accesses receiver

``` {.rust}
enum Option<T> { None, Some(T) } // algebraic data! generics!

impl<T> Option<T> {
    // &Option<T> -> bool
    fn is_some(&self) -> bool {
        match *self {
            Some(_) => true,
            None => false
        }
    }

    fn is_none(&self) -> bool {
        !self.is_some()
    }
}
```

## Method signatures: &mut self

"Mutates" receiver

``` {.rust}
enum Option<T> { None, Some(T) }

impl<T> Option<T> {
    // &mut Option<T> -> Option<T>
    fn take(&mut self) -> Option<T> {
        let mut src = None;
        swap(self, &mut src);
        src
    }
}
```

## Method signatures: self

<span class="strike">"Consumes"</span> Takes ownership of receiver

``` {.rust}
impl<T> Option<T> {
    // Option<T> -> T
    fn unwrap_or(self, default: T) -> T {
        match self {
            Some(x) => x,
            None => default
        }
    } // one of `self` or `default` dropped at end
}
```

----

```{.rust}
impl<T> Option<T> {
    fn is_some(&self) -> bool { ... }
    fn is_none(&self) -> bool { ... }
    fn take(&mut self) -> Option<T> { ... }
    fn unwrap_or(self, default: T) -> T { ... }
}
```

<!--
```rust
#[test]
fn demo_reading_protocol() {
    demo_subroutine(format!("World"));
}
```
-->

. . .

Some magic: [method invocations](http://is.gd/n8CaTJ)
automatically
do borrows *and* derefs as necessary on the receiver
("auto-ref").

. . .

```rust
fn demo_subroutine(name: String) {
    let mut hi = Some(format!("Hello {}", &name));
    assert!(hi.is_some());   // this is an `&hi`

    let grabbed = hi.take(); // this is an `&mut hi`
    assert!(hi.is_none());   // this is an `&hi`

    // and this consumes `grabbed`
    assert!(&grabbed.unwrap_or(name)[0..5] == "Hello");
}
```

# For the C++ fans in the audience: Smart Pointers {.center}

## "Smart" "Pointers" {.center}

* `Box<T>`: unique reference to `T` on
            (`malloc`/`free`-style) heap

* `Rc<T>`: shared ownership, thread-local

* `Arc<T>`: shared ownership, safe across threads

. . .

All of above deref to `&T`

## [`Deref` works here too](http://is.gd/J9RKXK) {.center}

```rust
#[test]
fn demo_rc_of_point() {
    use std::rc::Rc;
    let p1 = Rc::new(Point { x: 3, y: 4 });
    let p2 = p1.clone();
    assert_eq!(5, p1.distance_from_origin());
    assert_eq!(5, p2.distance_from_origin());

    // p1.x = 6; // (STATIC ERROR; cannot assign Rc contents)
}
```

## Other Deref fun

`Box<T>`, `Rc<T>`, `Arc<T>` all deref to `&T`

But we also have things like this:

``` {.rust}
pub enum Cow<'a, B> where B: ToOwned {
    Borrowed(&'a B),
    Owned(<B as ToOwned>::Owned)
}
```

which derefs to `&B`, even if it is in the `Owned` variant.

. . .

(Useful for APIs that want to delay the decision of whether to return
an owned value or a borrowed reference.)




# Lifetime Bindings {.center}

## [Lifetime Bindings 1](http://is.gd/YUeL9J) {.center}

We saw this kind of thing before:

```rust
#[test]
fn explicit_lifetime_binding_1() {
    fn print<'a>(ints: &'a Vec<i32>) {
        println!("v_1: {}", ints[1]);
    }
    let v1 = vec![1, 2, 3];
    print(&v1)
}
```

## [Lifetime Bindings 2](http://is.gd/VUhzbN)  { data-transition="fade-out" }

You can bind distinct lifetimes:

```rust
#[test]
fn explicit_lifetime_binding_2() {
    fn print<'a, 'b>(ptrs: &'a Vec<&'b i32>) {
        println!("v_1: {}", ptrs[1]);

    }
    let one = 1;
    let two = 2;
    let three = 3;
    let four = 4;
    let v1 = vec![&one, &two, &three];
    print(&v1)
}
```

## [Lifetime Bindings 3](http://is.gd/I22UHF)  { data-transition="fade" }

Encode constraints by reusing same lifetime:

```rust
#[test]
fn explicit_lifetime_binding_3() {
    fn print<'a, 'b>(ptrs: &'a mut Vec<&'b i32>, ptr: &'b i32) {
        println!("v_1: {}", ptrs[1]);
        ptrs.push(ptr);
    }
    let one = 1;
    let two = 2;
    let three = 3;
    let four = 4;
    let mut v1 = vec![&one, &two, &three];
    print(&mut v1, &four);
}
```

## [Lifetime Bindings 4](http://is.gd/5wOydB)  { data-transition="fade-in" }

Encode constraints by reusing same lifetime:

```rust
#[test]
fn explicit_lifetime_binding_4() {
    fn print<'a, 'b>(ptrs: &'a mut Vec<&'b i32>, ptr: &'b i32) {
        println!("v_1: {}", ptrs[1]);//~~~            ~~~
        ptrs.push(ptr);            //   |              |
    }                              // this must match that,
    let one = 1;                   // otherwise push is bogus
    let two = 2;
    let three = 3;
    let four = 4;
    let mut v1 = vec![&one, &two, &three];
    print(&mut v1, &four);
}
```

## [Lifetime Bindings 5](http://is.gd/JX2zpy)  { data-transition="fade-in" }

Compiler catches missing necessary constraints:

``` {.rust .compile_error}
#[test]
fn explicit_lifetime_binding_5() {
    fn print<'a, 'b, 'c>(ptrs: &'a mut Vec<&'b i32>, ptr: &'c i32) {
        println!("v_1: {}", ptrs[1]);  //  ~~~            ~~~
        ptrs.push(ptr);                //   |              |
    }                                  // this must match that,
    let one = 1;                       // otherwise push is bogus
}
```

``` {.fragment}
error: cannot infer an appropriate lifetime for automatic coercion
       due to conflicting requirements
        ptrs.push(ptr);
                  ^~~
help: consider using an explicit lifetime parameter as shown:
    fn print<'a, 'b>(ptrs: &'a mut Vec<&'b i32>, ptr: &'b i32)
```

## [Borrowed return values 1](http://is.gd/gKhfy0) { data-transition="fade-out" }

```rust
fn first_n_last<'a>(ints: &'a Vec<i32>) -> (&'a i32, &'a i32) {
    //                                      ~~~~~~~  ~~~~~~~
    (&ints[0], &ints[ints.len() - 1])
}
```

<!--
TODO: Exercise idea: Try to write `fn first_and_last_mut`. Why is it impossible
in general?
-->

. . .

```rust
#[test]
fn demo_borrowed_return_values() {
	let v = vec![1, 2, 3, 4];
	let (first, fourth) = first_n_last(&v);
	assert_eq!(*first, 1);
	assert_eq!(*fourth, 4);
}
```

(compiler ensures borrow `&v`{.rust} lasts long enough to satisfy
 reads of `first` and `fourth`)

## [Borrowed return values 2](http://is.gd/DzWgSs)  { data-transition="fade-in" }

``` {.rust .compile_error}
fn first_n_last<'a>(ints: Vec<i32>) -> (&'a i32, &'a i32) {
    //                    ~~~~~~~~ (hint)
    (&ints[0], &ints[ints.len() - 1])
}
```

Why doesn't this work?

``` {.fragment data-fragment-index="1" }
error: `ints` does not live long enough
    (&ints[0], &ints[ints.len() - 1])
      ^~~~
note: reference must be valid for the lifetime 'a ...
note: ...but borrowed value is only valid for the scope of
note:    parameters for function
```

. . .

caller chooses `'a`{.rust}; `fn` body must work for any such choice

(Parameters dropped at scope end; won't live long enough)

# Lifetime Elision {.center}

## All the `'a`{.rust}, `'b`{.rust}, ... are ugly {.center}

## [Lifetime Elision 1](http://is.gd/72HT0U) { data-transition="fade-out" }

```rust
#[test]
fn lifetime_elision_1() {
    fn print1<'a>(ints: &'a Vec<i32>) {
        println!("v_1: {}", ints[1]);
    }
    fn print2<'a, 'b>(ptrs: &'a Vec<&'b i32>) {
        println!("v_1: {}", ptrs[1]);

    }
    fn print3<'a, 'b>(ptrs: &'a mut Vec<&'b i32>, ptr: &'b i32) {
        println!("v_1: {}", ptrs[1]);
        ptrs.push(ptr);
    }
}
```

## [Lifetime Elision 2](http://is.gd/UvyycH) { data-transition="fade" }

```rust
#[test]
fn lifetime_elision_2() {
    fn print1    (ints: &   Vec<i32>) {
        println!("v_1: {}", ints[1]);
    }
    fn print2        (ptrs: &   Vec<&   i32>) {
        println!("v_1: {}", ptrs[1]);

    }
    fn print3<    'b>(ptrs: &   mut Vec<&'b i32>, ptr: &'b i32) {
        println!("v_1: {}", ptrs[1]);
        ptrs.push(ptr);
    }
}
```

## [Lifetime Elision 3](http://is.gd/Rm4qIc) { data-transition="fade-in" }

```rust
#[test]
fn lifetime_elision_3() {
    fn print1(ints: &Vec<i32>) {
        println!("v_1: {}", ints[1]);
    }
    fn print2(ptrs: &Vec<&i32>) {
        println!("v_1: {}", ptrs[1]);

    }
    fn print3<'b>(ptrs: &mut Vec<&'b i32>, ptr: &'b i32) {
        println!("v_1: {}", ptrs[1]);
        ptrs.push(ptr);
    }
}
```

# Generic items {.center}

## [Generic items 1](http://is.gd/Rd0aYF) { data-transition="fade-out" }

```rust
#[test]
fn generic_items_1() {
    fn push_twice<'b>(ptrs: &mut Vec<&'b i32>, ptr: &'b i32) {
        ptrs.push(ptr);
        ptrs.push(ptr);
    }
    let (one, two, three, four) = (1, 2, 3, 4);
    let mut v = vec![&one, &two, &three];
    push_twice(&mut v, &four);
}
```

This obviously generalizes beyond `i32`!

## [Generic items 2](http://is.gd/ehrrSy) { data-transition="fade-in" }

```rust
#[test]
fn generic_items_2() {
    fn push_twice<'b, T>(ptrs: &mut Vec<&'b T>, ptr: &'b T) {
        ptrs.push(ptr);
        ptrs.push(ptr);
    }
    let (one, two, three, four) = (1, 2, 3, 4);
    let mut v = vec![&one, &two, &three];
    push_twice(&mut v, &four);
}
```

This is going so smoothly; lets try printing `v_1` again!

## [Generic items 3](http://is.gd/bINY38)

``` { .rust .compile_error }
#[test]
fn generic_items_3() {
    fn push_twice<'b, T>(ptrs: &mut Vec<&'b T>, ptr: &'b T) {
        println!("v_1: {}", ptrs[1]);
        ptrs.push(ptr);
        ptrs.push(ptr);
    }
    let (one, two, three, four) = (1, 2, 3, 4);
    let mut v = vec![&one, &two, &three];
    push_twice(&mut v, &four);
}
```

```{.fragment}
error: trait `core::fmt::Display` not implemented for the type `T`
        println!("v_1: {}", ptrs[1]);
                            ^~~~~~~
```

(Reminder: Rust is not C++)

# Trait-bounded polymorphism {.center}

## [Trait-bounded polymorphism](http://is.gd/Xy9l7N) {.center}

```rust
trait Dimensioned {
    fn height(&self) -> u32;
    fn width(&self) -> u32;
}

fn stacked_height<S>(v: &[S]) -> u32 where S: Dimensioned {
    let mut accum = 0;
    for s in v { accum += s.height() }
    accum
}
```

## [Trait Impls](http://is.gd/Xy9l7N) {.center}

```rust
struct Rect { w: u32, h: u32 }
struct Circle { r: u32 }

impl Dimensioned for Rect {
    fn height(&self) -> u32 { self.h }
    fn width(&self) -> u32 { self.w }
}

impl Dimensioned for Circle {
    fn height(&self) -> u32 { self.r * 2 }
    fn width(&self) -> u32 { self.r * 2 }
}
```

## [Traits in Action](http://is.gd/Xy9l7N) {.center}

```rust
impl Rect {
    fn square(l: u32) -> Rect { Rect { w: l, h: l } }
}
impl Circle {
    fn with_radius(r: u32) -> Circle { Circle { r: r } }
}

#[test]
fn trait_bounded_polymorphism() {
    let squares = [ Rect::square(1), Rect::square(2) ];
    let circles = [ Circle::with_radius(1), Circle::with_radius(2)];
    assert_eq!(stacked_height(&squares), 3);
    assert_eq!(stacked_height(&circles), 6);
}
```

# Generics do not suffice {.center}

## [Generics do not suffice](http://is.gd/S5fXjQ) {.center}

``` {.rust .compile_error}
#[test]
fn parametric_fail() {
    let shapes = [Rect::square(1), Circle::with_radius(2)];
    assert_eq!(stacked_height(&shapes), 5);
}
```

``` {.fragment}
error: mismatched types:
 expected `Rect`,
    found `Circle`
    let shapes = [Rect::square(1), Circle::with_radius(2)];
                                   ^~~~~~~~~~~~~~~~~~~~~~
```

## [Uniformity of `T` in `Vec<T>` is why](http://is.gd/fA53YK) {.center}

``` {.rust .compile_error}
struct Rect { w: u32, h: u32 }
struct Circle { r: u32 }

fn parametric_fail() {
    let shapes = [Rect::square(1), Circle::with_radius(2)];
    //  ~~~~~~    ~~~~~~~~~~~~~~~  ~~~~~~~~~~~~~~~~~~~~~~
    //    |              |                    |
    //    |       This is 8 bytes     This is 4-bytes
    //    |
    //  There's no uniform array
    //  type to hold both in-line.
}
```

## This is a job for ... {.center}

### Object-Oriented Programming! {.fragment}

## [Traits as Objects 1](http://is.gd/akO1vd)

```rust
fn stacked_obj_refs(v: &[&Dimensioned]) -> u32 {
    let mut accum = 0;
    for s in v { accum += s.height() }
    accum
}

#[test]
fn demo_objs_1() {
    let r = Rect::square(1);
    let c = Circle::with_radius(2);
    let shapes: [&Dimensioned; 2] = [&r, &c];
    assert_eq!(stacked_obj_refs(&shapes), 5);
}
```

## [Traits as Objects 2](http://is.gd/BbBlOz)

```rust
fn stacked_obj_boxes(v: &[Box<Dimensioned>]) -> u32 {
    let mut accum = 0;
    for s in v { accum += s.height() }
    accum
}

#[test]
fn demo_objs_2() {
    let shapes: [Box<Dimensioned>; 2] =
        [Box::new(Rect::square(1)), Box::new(Circle::with_radius(2))];
    assert_eq!(stacked_obj_boxes(&shapes), 5);
}
```

# OOP is nice; how about Functional Programming? {.center}


## Closures 1 {.center}

* Can pass functions around as first class entities

* Functions can *close* over externally defined state

Reminder from Javascript:

`closures.js`{.filename}
```javascript
function add3(x) { return x + 3; }

// take function as parameter:
function do_twice(f, y) { return f(f(y)); }

// return function that references outer parameter `z`
function make_adder(z) {
    return function(w) { return w + z; };
}

var add4 = make_adder(4);
var ten = do_twice(add4, 2);
```

## Closures 2  {.center}

  * In (classic) Javascript, closure syntax is:
    ```javascript
    function (args, ...) { body; ... }
    ```
    where `body` can refer to things from outside.

  * In Rust, the analogous closure expression syntax is:

    ``` {.rust}
    |args, ...| { body; ... }
    ```
    with a few extra details:

. . .

  * opt. `move`{.rust} (forces capture-by-move)

  * opt. arg. and return types (inferred when omitted)

## [Closures 3](http://is.gd/Kc3f8v) {.center}

```rust
#[test]
fn demo_closure() {
    fn add3(x: i32) -> i32 { x + 3 } // <- fn, *not* a closure
    fn do_twice1<F:Fn(i32) -> i32>(f: F, x: i32) -> i32 { f(f(x)) }
    //             ~~~~~~~~~~~~~~ closure type
    fn do_twice2(f: &Fn(i32) -> i32, x: i32) -> i32 { f(f(x)) }

    fn make_adder(y: i32) -> Box<Fn(i32) -> i32> {
        Box::new(move |x| { x + y })
            //   ~~~~~~~~~~~~~~~~~~ closure expression
    }

    let add4 = make_adder(4);
    let six = do_twice1(&add3, 0); let ten = do_twice1(&*add4, 2);
    assert_eq!((six, ten), (6, 10));
    let six = do_twice2(&add3, 0); let ten = do_twice2(&*add4, 2);
    assert_eq!((six, ten), (6, 10));
}
```

# Interior Mutability {.center}

## What about mutation outside of `&mut`{.rust}? {.center}

## Interior Mutability: `Cell` and `RefCell` structs {.center}

 * Both types have mutator methods that take `&self`{.rust}

     * *not* `&mut self`{.rust}

 * `Cell<T>`: has `get` and `set` methods, but only accepts `T:Copy`

 * `RefCell<T>` handles all types `T`, but *dynamically*
   *enforces* the rules.

     * `borrow` method returns read-only `Ref` (many-readers),
        and `panic!`{.rust}'s on outstanding mut-borrow

     * `borrow_mut` method returns read/write `RefMut`,
        and `panic!`{.rust}'s on any outstanding borrow.

## Demos of `Cell` and `RefCell` types {.center}

## [`Cell` working:](http://is.gd/3fa2J6)

```rust
#[test]
fn demo_cell() {
    use std::cell::Cell;
    let c = Cell::new(3);
    let p1: &Cell<i32> = &c;
    let p2: &Cell<i32> = &c;
    assert_eq!(p2.get(), 3);
    p1.set(4);
    assert_eq!(p2.get(), 4);
}
```

## [`Cell` cannot hold non-`Copy` data](http://is.gd/EGXBTt)

``` {.rust .compile_error}
#[test]
fn demo_cell_vec_no_work() {
    use std::cell::Cell;
    let c = Cell::new(vec![1,2,3]);
    let p1: &Cell<Vec<i32>> = &c;
    let p2: &Cell<Vec<i32>> = &c;
    assert_eq!(p2.get(), [1,2,3]);
    p1.set(vec![4,5,6]);
    assert_eq!(p2.get(), [4,5,6]);
}
```

``` {.fragment}
error: the trait `core::marker::Copy` is not implemented
       for the type `collections::vec::Vec<_>` [E0277]
    let c = Cell::new(vec![1,2,3]);
            ^~~~~~~~~
```

## [`RefCell` handles all data](http://is.gd/OwUeLZ)

```rust
#[test]
fn demo_refcell_vec() {
    use std::cell::RefCell;
    let c = RefCell::new(vec![1,2,3]);
    let p1: &RefCell<Vec<i32>> = &c;
    let p2: &RefCell<Vec<i32>> = &c;
    assert_eq!(*p2.borrow(), [1,2,3]);
    p1.borrow_mut().push(4);
    assert_eq!(*p2.borrow(), [1,2,3,4]);
}
```

## [`RefCell` dynamically errors if misused](http://is.gd/2vcGT0)

<!--
```rust
#[should_panic]
```
-->

```rust
#[test]
fn demo_refcell_vec_no_work() {
    use std::cell::RefCell;
    let c = RefCell::new(vec![1,2,3]);
    let p1: &RefCell<Vec<i32>> = &c;
    let p2: &RefCell<Vec<i32>> = &c;
    let b2 = p2.borrow();
    assert_eq!(*b2, [1,2,3]);
    p1.borrow_mut().push(4);
    assert_eq!(*b2, [1,2,3,4]);
}
```

``` {.fragment}
---- a01_start::demo_refcell_vec_no_work stdout ----
    thread 'a01_start::demo_refcell_vec_no_work' panicked
        at 'RefCell<T> already borrowed'
```

. . .

(This is *not* unsound!)

# Destructors {.center}

## The `Drop` trait {.center}

If a data-type is represents some resource with associated
cleanup actions, then it should implement `Drop`

```rust
struct Yell { name: &'static str }
impl Drop for Yell {
    fn drop(&mut self) {
        println!("My name is {}; ", self.name);
    }
}
```

## [Silly example of Yelling](http://is.gd/X3b6pA)

```rust
fn main() {
    let bob = Yell { name: "Bob" };
    let carol = Yell {
        name: {
            let carols_dad = Yell { name: "David" };
            "Carol"
        } // end of scope: carols_dad is dropped
    };
    let ed = Yell { name: "Ed" };
    let frank = Yell { name: panic!("What is Frank's name?") };
    println!("We never get to Gregor");
    let gregor = Yell { name: "Gregor" };
}
```

. . .

prints:

```
My name is David;
thread '<main>' panicked at 'What is Frank's name?', <anon>:16
My name is Ed;
My name is Carol;
My name is Bob;
```

## More realistic example: `Rc` implementation

``` {.rust}
struct RcBox<T> { strong: Cell<usize>, value: T }
pub struct Rc<T> { _ptr: NonZero<*mut RcBox<T>> }

impl<T: ?Sized> Clone for Rc<T> {
    fn clone(&self) -> Rc<T> {
        self.inc_strong();
        Rc { _ptr: self._ptr }
    }
}

impl<T> Drop for Rc<T> {
    fn drop(&mut self) {
        unsafe {
            let ptr = *self._ptr;
            self.decrement_count();
            if self.current_count() == 0 {
                drop_in_place(&mut (*ptr).value);
                deallocate(ptr);
            }
        }
    }
}
```
