# The End of It (15 min)

## Child, what day is it?

## Epochery

```rust
pub fn f() -> Result<i32, i32> {
    let collapse: Result<_, i32> = do catch {
        g()?;
        h()?;
        Ok(())
    };
    assert!(collapse.is_err());
    Ok(3)
}

fn g() -> Result<(), i32> { Err(0) }

fn h() -> Result<(), i32> { Err(1) }
```
