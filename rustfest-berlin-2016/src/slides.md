% Subtyping in Rust and Clarke's Third Law
% Felix Klock (`@pnkfelix`), Mozilla
% Rust Fest, Berlin, 17 September 2016

# Bienvenue {.center}

```rust
#![allow(dead_code, unused_variables)]
```

## Hello World

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

## Aside: Are Magicians Liars?

# Coercions

## Gotchas

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
