# Rust: What? Why? (How?)

## Rust: What

New systems programming language

. . .

low-level control

 * fast; FFI interface; data layout control
 * compete (and interface with) with C/C++

. . .

mix the classic hits of PL

 * user-defined iterators, RAII, objects with vtable method dispatch, generics / F-bounded polymorphism, algebraic data types, affine types, et cetera

. . .

memory-safe, data-race free

 * fearless concurrency

## Rust: Where

### Language and API docs

  * All linked from top of [http://www.rust-lang.org/]

  * Starting points
    * The Book: [https://doc.rust-lang.org/stable/book/]
    * Standard API: [https://doc.rust-lang.org/stable/std/]

### Playpen

  * [http://play.rust-lang.org/]


[http://www.rust-lang.org/]: http://www.rust-lang.org/

[https://doc.rust-lang.org/stable/book/]: https://doc.rust-lang.org/stable/book/

[https://doc.rust-lang.org/stable/std/]: https://doc.rust-lang.org/stable/std/


[http://play.rust-lang.org/]: http://play.rust-lang.org/


## Rust: Why?  { .left_align }

. . .

Competitive Advantage

C/C++ impedes ability to compete in the browser market

. . .

Fast experimentation & deployment

## Servo { .left_align }

  * browser

  * implementation research platform

  * written in Rust

. . .

Experiments

  * parallel paint

  * parallel layout

  * parallel css selector matching

## Rust: A note on goals { .left_align }

What is soundness?

Segfault is not okay

But: "clean" shutdown (e.g. on internal error) *is* okay

 * Rust has `panic!`{.rust}, `assert!`{.rust}

 * A panic unwinds the stack, cleaning up resources

Rust uses dynamic checks where appropriate

 * But prefer static assurance when feasible

## Rust: How? { .left_align }

The Big Ideas

 * Ownership + Move Semantics

 * Borrowing

 * Lifetimes

## Rust: How? { .left_align }

The Big Ideas

 * Ownership + Move Semantics (resource control)

 * Borrowing (brings back reference semantics)

 * Lifetimes (constraints between references)

## Move Semantics

Create (and modify) owned:
```rust
#[test]
pub fn create_owned() {
    let mut vec = Vec::new();         //  + (`vec` created ...
    vec.push(2000);                   //  |  ... and
    vec.push( 400);                   //  |       also
    vec.push(  60);                   //  |        mutated
    println!("vec: {:?}", vec);       //  |
    let the_sum = sum(vec);           //  o ... and moved)
    // (access to `vec` is static error starting here)
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

## Moved data errors

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

``` {.compile_error .fragment}
error: use of moved value: `moving_value` [E0382]
    println!("moving_value: {:?}", moving_value);
                                   ^~~~~~~~~~~~

note: `moving_value` moved here because it has type
      `collections::vec::Vec<i32>`, which is non-copyable
    let tuple = (moving_value, copied_value);
                 ^~~~~~~~~~~~
```


## Moves insufficient on their own

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

``` {.fragment}
error: use of moved value: `vec` [E0382]
    let result2 = other_calculation(vec); // oops
                                    ^~~
note: `vec` moved here because it has type
      `collections::vec::Vec<i32>`, which is non-copyable
    let result1 = some_vec_calculation(vec); // <-- `vec` moved here
                                       ^~~
```

## Learning to Share

Borrowing: *Lend* references to values

```rust
#[test]
fn hooray_for_borrows() {
    let vec = expensive_vector_computation();

    let result1 = some_vec_calculation(&vec); // <-- lend out `vec`

    let result2 = other_calculation(&vec); // <-- lend again, no prob

    combine(result1, result2);

} // (`vec` is destroyed/freed aka "dropped" here)
```

## Mo' features, mo' problems

* Why are safety violations generally hard to detect?

. . .

* It is due to *aliasing*

* Borrows *reintroduce* aliasing

### Q: How to ensure safety in presence of aliasing?  {.fragment}

### A: Restrict the aliasing {.fragment}

## Restricted aliasing

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

## Examples of Types

```rust
fn read(v: &Vec<String>) -> String {
    let first: &String = &v[0];
    println!("v[0]: {}", first);
    return first.clone();
}
```
. . .

Some magic: method calls automatically
do all necessary borrows *and* derefs on the receiver
("auto-ref").

<!-- TODO: show how returning &str breaks down) -->

```rust
fn modify(v: &mut Vec<String>, name: &str) {
    let freshly_created = format!("Hello {}", name);
    v.push(freshly_created);
}

fn consume(v: Vec<String>) -> String {
    for s in v { return s; }
    panic!("v was empty?!?");
}
```

## Examples of types

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

## Reading protocol from signatures

* `self`{.rust}: *consumes* receiver
* `&self`{.rust}: *accesses* receiver
* `&mut self`{.rust}: *mutates* receiver

## Reading protocol from signatures: &self

``` {.rust}
enum Option<T> { None, Some(T) }

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

## Reading protocol from signatures: &mut self

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

## Reading protocol from signatures: self

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

```rust
fn demo_subroutine(name: String) {
    let mut hi = Some(format!("Hello {}", &name));
    assert!(hi.is_some());
    let grabbed = hi.take();
    assert!(hi.is_none());
    assert!(&grabbed.unwrap_or(name)[0..5] == "Hello");
}
```


## Immutable borrows

## Borrowing (immutably) { data-transition="fade-out" }

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

## Scopes and Lifetimes { data-transition="fade-in" }

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
}                           //  +    +    +    +
```

``` {.rust}
fn foo<'a>(v: &'a Vec<i32>) { println!("v[1]: {}", v[1]); }
```

Each borrow selects "appropriate" lifetime `'a`.

## Borrow Checking Prevents Errors { data-transition="fade-out" }

``` {.rust .compile_error}
fn borrow_checking_prevents_errors() {

    let v1 = vec![1, 2, 3];      //        +
                                 //        |
    let r1 = &v1;                //  +    'v1
                                 //  |     |
    consume(v1);                 // 'r1   (moved)
    foo(r1);                     //  |
}                                //  +
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

## Lifetimes and Lexical Scope { data-transition="fade-in" }

``` {.rust .compile_error}
fn borrow_checking_may_seem_simple_minded() {

    let v1 = vec![1, 2, 3];      //        +
                                 //        |
    let r1 = &v1;                //  +    'v1
                                 //  |     |
    consume(v1);                 // 'r1   (moved)
    // (no call to read)         //  |
}                                //  +
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

## Built on lexical scopes, but non-trivial { data-transition="slide-in fade-out" }

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
    };                      //  +  +   |    |
    // (maybe mutate `v1`   //     |   |    |
    // here someday?)       //     |   |    |
                            //    'r2  |    |
    foo(r2);                //     |   |    |
}                           //     +   +    +
```

How long should the borrow `&v1` last?

<div class="notes">
In this case, the borrow marked "A" only needs to last
long enough for the call to `foo(r1)`; after that point,
the borrow is never needed.
</div>

## Built on lexical scopes, but non-trivial { data-transition="fade-in slide-out" }

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
    };                      //  +  +   |    |
    // (maybe mutate `v1`   //     |   |    |
    // here someday?)       //     |   |    |
                            //    'r2  |    |
    foo(r2);                //     |   |    |
}                           //     +   +    +
```

How long should the borrow `&v1` last now?

<div class="notes">
In this case, the borrow marked "A" needs to last longer!
The value of `r1` is being copied into `r2` on the
line marked "B", so the borrow marked "A" needs to
include the scope of both `'r1` and `'r2`.
</div>

## imm-borrows: can be copied freely { data-transition="slide-out" }

(super super useful to be able to share readable data!)

## imm-borrows: can be copied freely { data-transition="slide-in fade-out" }

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

```{ .fragment .compile_error }
error: cannot borrow immutable borrowed content `*v` as mutable
    v.push(4);
    ^
```

## imm-borrows: can be copied freely { data-transition="fade-in slide-out" }

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

## "... I want my imperative algorithms! ..."


## `&mut`{.rust} borrows


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

## What does `&mut`{.rust} mean (crucial) { data-transition="zoom-out" }

For many (but not all) types, safe mutation *requires* exclusive access

Any operation requiring exclusive access should either:

  * take ownership, or,

  * take an `&mut`{.rust}-reference

## Exclusive Access versus Ownership

```rust
fn take_by_value(v: Vec<i32>) { let mut v = v; v.push(4);  }
fn take_mut_borrow(b: &mut Vec<i32>) { b.push(10); }
// seemingly similar in power
```
. . .

``` {.rust .compile_error}
#[test]
fn demo_exclusive_access_versus_ownership() {
    let mut v1 = vec![1, 2, 3];
    let mut v2 = vec![7, 8, 9];
    take_by_value(v1);
    take_mut_borrow(&mut v2);
    println!("v1: {:?} v2: {:?}", v1, v2);
}
```

``` { .fragment .compile_error }
error: use of moved value: `v1` [E0382]
    println!("v1: {:?} v2: {:?}", v1, v2);
                                  ^~
note: `v1` moved here
    take_by_value(v1);
                  ^~
```

. . .

ownership ⇒ power + responsibility (for dropping)

. . .

`&mut` ⇒ power without responsibility; (can only *swap*)


# `&mut`{.rust} safety enforcement

## Data has at most one `&mut`{.rust} borrow { data-transition="slide-in fade-out" }

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

``` { .fragment .compile_error }
error: cannot borrow `v1` as mutable more than once at a time
    take2(&mut v1, &mut v1);
                        ^~
note: previous borrow of `v1` occurs here; the mutable borrow
      prevents subsequent moves, borrows, or modification of
      `v1` until the borrow ends
    take2(&mut v1, &mut v1);
               ^~


```

## Cannot alias `&mut`{.rust}-borrowed data { data-transition="fade" }

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

``` { .fragment .compile_error }
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

## `&mut T`{.rust} is non-copy { data-transition="fade-in slide-out" }

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

``` { .fragment .compile_error }
error: use of moved value: `*b` [E0382]
    take2(b, c);
          ^
note: `b` moved here because it has type
      `&mut collections::vec::Vec<i32>`, which is moved by default
    let c = b;
        ^
```

(ensures exclusive access)


## "Smart" "Pointers"

* `Box<T>`: owned, `Send`

* `Rc<T>`: shared, not TODO

* `Arc<T>`: shared, TODO

```rust
#[test]
fn demo_smart_pointers() {

}
```
