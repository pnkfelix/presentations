## Ligatures?

```rust
fn foo(x: (u32, u32)) -> u32 {
    match x {
        (x, 1) => x,
        (1, y) => y,
        (x, y) => x + y + 3,
    }
}
```

What `about => here`{.rust}
