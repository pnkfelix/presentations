# Ownership

(Time: 10 minutes)

 * Goal #1: Get code below ([playpen][]) to compile.

 * Goal #2: Convert the code so that it prints
   `Removing vowels from "Rustaceans" yields "Rstcns"`.

 * Extra-credit: Can you accomplish the previous goal
   without copying any data (e.g. no calls to `.clone()`).

 * Goal #3: This code is sound, but it is still buggy; identify and fix the bug.
   Hint: <a><span id="hint1" class="hint" onclick="var h = document.getElementById('hint1'); h.style.color = (h.style.color == 'inherit') ? 'transparent' : 'inherit';">What happens if you pass `"SNOBOL Throwers"` instead?</span></a>

```rust
fn main() {
    let (adjective, name) = two_words();
    let name = format!("{} {}", adjective, name);
    print_out(name);
}

fn two_words() -> (String, String) {
    (format!("fellow"), format!("Rustaceans"))
}

fn remove_vowels(name: String) -> String {
    // Goal #1: What is needed here to make this compile?
    let output = String::new();
    for c in name.chars() {
        match c {
            'a' | 'e' | 'i' | 'o' | 'u' => {
                // skip vowels
            }
            _ => {
                output.push(c);
            }
        }
    }
    output
}

fn print_out(name: String) {
    let devowelized_name = remove_vowels(name);
    println!("Removing vowels yields {:?}", devowelized_name);

    // Goal #2: What happens when you uncomment the following
    // line? Can you change the code above so that this next line?
    // println!("Removing vowels from {:?} yields {:?}",
    //          name, devowelized_name);

    // Extra credit: Can you do it without copying any data?
    // (Using only ownership transfer)
}
```
[playpen]: https://play.rust-lang.org/?code=fn%20main%28%29%20%7B%0A%20%20%20%20let%20%28adjective%2C%20name%29%20%3D%20two_words%28%29%3B%0A%20%20%20%20let%20name%20%3D%20format%21%28%22%7B%7D%20%7B%7D%22%2C%20adjective%2C%20name%29%3B%0A%20%20%20%20print_out%28name%29%3B%0A%7D%0A%0Afn%20two_words%28%29%20-%3E%20%28String%2C%20String%29%20%7B%0A%20%20%20%20%28format%21%28%22fellow%22%29%2C%20format%21%28%22Rustaceans%22%29%29%0A%7D%0A%0Afn%20remove_vowels%28name%3A%20String%29%20-%3E%20String%20%7B%0A%20%20%20%20%2F%2F%20Goal%20%231%3A%20What%20is%20needed%20here%20to%20make%20this%20compile%3F%0A%20%20%20%20let%20output%20%3D%20String%3A%3Anew%28%29%3B%0A%20%20%20%20for%20c%20in%20name.chars%28%29%20%7B%0A%20%20%20%20%20%20%20%20match%20c%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%27a%27%20%7C%20%27e%27%20%7C%20%27i%27%20%7C%20%27o%27%20%7C%20%27u%27%20%3D%3E%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%2F%2F%20skip%20vowels%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%20%20%20%20_%20%3D%3E%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20output.push%28c%29%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%7D%0A%20%20%20%20output%0A%7D%0A%0Afn%20print_out%28name%3A%20String%29%20%7B%0A%20%20%20%20let%20devowelized_name%20%3D%20remove_vowels%28name%29%3B%0A%20%20%20%20println%21%28%22Removing%20vowels%20yields%20%7B%3A%3F%7D%22%2C%20devowelized_name%29%3B%0A%0A%20%20%20%20%2F%2F%20Goal%20%232%3A%20What%20happens%20when%20you%20uncomment%20the%20following%0A%20%20%20%20%2F%2F%20line%3F%20Can%20you%20change%20the%20code%20above%20so%20that%20this%20next%20line%3F%0A%20%20%20%20%2F%2F%20println%21%28%22Removing%20vowels%20from%20%7B%3A%3F%7D%20yields%20%7B%3A%3F%7D%22%2C%0A%20%20%20%20%2F%2F%20%20%20%20%20%20%20%20%20%20name%2C%20devowelized_name%29%3B%0A%0A%20%20%20%20%2F%2F%20Extra%20credit%3A%20Can%20you%20do%20it%20without%20copying%20any%20data%3F%0A%20%20%20%20%2F%2F%20%28Using%20only%20ownership%20transfer%29%0A%7D&version=nightly
