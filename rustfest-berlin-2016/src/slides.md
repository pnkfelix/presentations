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

E.g. if you have
```rust
trait Input { }
impl Input for [u8] { }
fn process1(input: &[u8]) { }
fn process2<I>(input: &I) where I: Input { }
```

consider

```rust
fn foo() { process1(&vec![1, 2, 3]); }
```

but this fails

```rust
#[cfg(off)]
fn bar() { process2(&vec![1, 2, 3]); }
```

# Conclusion  {.center}

## More Info
