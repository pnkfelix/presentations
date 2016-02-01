# Borrowing

Time: 10 minutes

  * Goal: convert `strcat` function below ([playpen][]) so that it uses
    borrowing, not ownership.

    (As part of this, we no longer want to build up a whole new string;
     so get rid of `str3` and make the code just change `str1`
     in-place.)

    Hint: Getting the syntax right can be a bit tricky if you've never written in Rust before.
    Click on the hidden text below for some guidance.
    <a><div id="hint2" class="hint"
    onclick="var h = document.getElementById('hint2'); h.style.color = (h.style.color == 'inherit') ? 'transparent' : 'inherit';">
    You want to change the signature
    of `join_words` as follows: `fn strcat(prefix: &mut String, suffix: &String) { ... }`

    Now `prefix` is a mutable reference to some `String` on the caller's side.
    We need a mutable reference so we can push new content onto the string.

    `suffix` is a shared reference; a shared reference suffices because we will only
    *read* from `suffix`.

    Note the return value has also changed; since we are going to be mutating `prefix` in
    place, we no longer need to return anything.
    </div></a>

  * Question (also in code): now that you've converted `strcat`, what
    happens if you call `strcat` using the same string instance for
    `prefix` and `suffix`, i.e. `strcat(s, s)`? Why?

```rust
pub fn main() {
    let str1 = format!("fellow ");
    let str2 = format!("Rustaceans");
    let str3 = strcat(str1, str2);
    println!("{}", str3);
}

/// Concatenate `suffix` onto the end of `prefix`.
fn strcat(mut prefix: String, suffix: String) -> String {
    for ch in suffix.chars() {
        prefix.push(ch);
    }
    prefix
}

// Challenge: Convert `strcat` to use borrowing, not ownership.

// Question: Now that you've converted `strcat`, what happens if you
// call `strcat` using the same string for `prefix` and `suffix`?
// Why?
```
[playpen]: https://play.rust-lang.org/?code=pub%20fn%20main%28%29%20%7B%0A%20%20%20%20let%20str1%20%3D%20format%21%28%22fellow%20%22%29%3B%0A%20%20%20%20let%20str2%20%3D%20format%21%28%22Rustaceans%22%29%3B%0A%20%20%20%20let%20str3%20%3D%20strcat%28str1%2C%20str2%29%3B%0A%20%20%20%20println%21%28%22%7B%7D%22%2C%20str3%29%3B%0A%7D%0A%0A%2F%2F%2F%20Concatenate%20%60suffix%60%20onto%20the%20end%20of%20%60prefix%60.%0Afn%20strcat%28mut%20prefix%3A%20String%2C%20suffix%3A%20String%29%20-%3E%20String%20%7B%0A%20%20%20%20for%20ch%20in%20suffix.chars%28%29%20%7B%0A%20%20%20%20%20%20%20%20prefix.push%28ch%29%3B%0A%20%20%20%20%7D%0A%20%20%20%20prefix%0A%7D%0A%0A%2F%2F%20Challenge%3A%20Convert%20%60strcat%60%20to%20use%20borrowing%2C%20not%20ownership.%0A%0A%2F%2F%20Question%3A%20Now%20that%20you%27ve%20converted%20%60strcat%60%2C%20what%20happens%20if%20you%0A%2F%2F%20call%20%60strcat%60%20using%20the%20same%20string%20for%20%60prefix%60%20and%20%60suffix%60%3F%0A%2F%2F%20Why%3F&version=nightly
