```rust
static S_2_0: [usize; 2] = [2, 0];
pub fn main() {
    let s_1_0_2 = [1, 0, 2];
    let first_choice: &[usize] = choose(&s_1_0_2, &S_2_0);
    println!("first_choice: {:?}", first_choice);

    let second_choice = subdata_then_choose();
    println!("second_choice: {:?}", second_choice);
}

// EXERCISE 1: In `fn choose`, remove the `<'a>` and all occurrences
// of `'a` from the signature of `fn choose`. What happens? Why?

fn choose<'a>(x: &'a [usize], y: &'a [usize]) -> &'a [usize] {
    // A semi-arbitrary predicate between the two inputs
    if y[x[0]] > x[y[0]] {
        x
    } else {
        y
    }
}

// EXERCISE 2: In `fn subdata_then_choose`, remove `return &ARRAY;`
// (marked `XXX`) and uncomment alternative code there. What happens
// when you try to compile this? What are some ways to address this?

fn subdata_then_choose<'a>() -> &'a [usize]  {
    return &ARRAY; // XXX
    /*

    let s_1_0_0 = [1, 0, 0];
    let s_0_2_0 = [0, 2, 0];
    choose(&s_1_0_0, &s_0_2_0)

    */
}

static ARRAY : [usize; 3] = [0,1,2];

const C_2_0: [usize; 2] = [2, 0];

// (EXTRA CREDIT)
// EXERCISE 3: In `fn main`, replace `&S_2_0` with `&C_2_0`.
//
//   * Investigate: why does the code stop compiling?
//
//   * How could we address this (within the body of `fn main` alone)?
//
//   * Why did the original code with `S_2_0` compile?
```
