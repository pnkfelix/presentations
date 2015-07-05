# Rust: What? Why? (How?)

## Rust: What

New systems programming language

fast

. . .

memory-safe

. . .

data-race free

## Rust: Why?  { .left_align }

### Competitive Advantage

C/C++ impedes ability to compete in the browser market

. . .

### Servo

  * a browser implementation research platform

  * written in Rust

### Fast experimentation & deployment

  * parallel paint

  * parallel layout

  * parallel css selector matching

## Concurrency

```rust
fn demo() -> u32 {
    3
}
```

<!--
```rust
#[test]
fn test_demo() {
    assert_eq!(demo(), 3);
}
```
-->
