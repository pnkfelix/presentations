## Memory Safety

``` {.rust .compile_error}
fn this_wont_compile(v: &mut Vec<i32>) -> i32 {
    let mut sum = 0;
    for &i in v.iter() {
        sum += i;
        if i > 0 { v.push(0); }
    }
    return sum;
}
```

([playpen][wont_compile])

<!--
```rust
fn this_wont_compile(v: &mut Vec<i32>) -> i32 {
    let mut sum = 0;
    for &i in v.iter() {
        sum += i;
        if i > 0 { v.push(0); }
    }
    return sum;
}

fn main() {}
```
-->
[wont_compile]: https://play.rust-lang.org/?code=fn%20this_wont_compile%28v%3A%20%26mut%20Vec%3Ci32%3E%29%20-%3E%20i32%20%7B%0A%20%20%20%20let%20mut%20sum%20%3D%200%3B%0A%20%20%20%20for%20%26i%20in%20v.iter%28%29%20%7B%0A%20%20%20%20%20%20%20%20sum%20%2B%3D%20i%3B%0A%20%20%20%20%20%20%20%20if%20i%20%3E%200%20%7B%20v.push%280%29%3B%20%7D%0A%20%20%20%20%7D%0A%20%20%20%20return%20sum%3B%0A%7D%0A%0Afn%20main%28%29%20%7B%7D&version=nightly

``` {.fragment}
<anon>:5:20: 5:21 error: cannot borrow `*v` as mutable because it is
                         also borrowed as immutable [E0502]
<anon>:5         if i > 0 { v.push(0); }
                            ^~
<anon>:3:15: 3:16 note: previous borrow of `*v` occurs here; the
                        immutable borrow prevents subsequent moves or
                        mutable borrows of `*v` until the borrow ends
<anon>:3     for &i in v.iter() {
                       ^~
```
