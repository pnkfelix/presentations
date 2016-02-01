<!--
```rust
#[inline(never)]
/// sums all positive values in `v`
fn sum_pos(v: &[i32]) -> i32 {
    let mut sum = 0;
    for i in v.iter().filter(|i| **i > 0) {
        sum += *i;
    }
    sum
}

fn main() {
    println!("sum: {}", sum_pos(&[-13, 10, 20, 30, 40]))
}
```
-->
[sum_pos]: https://play.rust-lang.org/?code=%23%5Binline%28never%29%5D%0A%2F%2F%2F%20sums%20all%20positive%20values%20in%20%60v%60%0Afn%20sum_pos%28v%3A%20%26%5Bi32%5D%29%20-%3E%20i32%20%7B%0A%20%20%20%20let%20mut%20sum%20%3D%200%3B%0A%20%20%20%20for%20i%20in%20v.iter%28%29.filter%28%7Ci%7C%20**i%20%3E%200%29%20%7B%0A%20%20%20%20%20%20%20%20sum%20%2B%3D%20*i%3B%0A%20%20%20%20%7D%0A%20%20%20%20sum%0A%7D%0A%0Afn%20main%28%29%20%7B%0A%20%20%20%20println%21%28%22sum%3A%20%7B%7D%22%2C%20sum_pos%28%26%5B-13%2C%2010%2C%2020%2C%2030%2C%2040%5D%29%29%0A%7D&version=nightly

<!--
     HACK: commenting out code block but it will still get rendered into link below.
     (The code is presented and linked in a01_abstraction.md.)

     Further more, the code block will still get converted into
     actual Rust code in the current module by `tango`, so
     that is why this is in a file distinct from
     a01_abstraction.md.
-->
