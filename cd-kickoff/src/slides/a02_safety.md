## Memory Safety

```rust
fn this_wont_compile(v: &mut Vec<i32>) -> i32 {
    let mut sum = 0;
    for &i in v.iter() {
        sum += i;
        if i > 0 { v.push(0); }
    }
}
```
