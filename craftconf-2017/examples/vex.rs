fn gather_negatives(input: &Vec<i32>, output: &mut Vec<i32>) -> isize {
    let mut count = 0;
    for it in input {
        let val = *it;
        if val < 0 { output.push(-val); count += 1; }
    }
    return count;
}

fn main() {
    let values = [-1, 2, 3, -4, -5, -6, -7, -8, -9, 10];
    let mut input: Vec<i32> = Vec::new();
    let mut myvector = Vec::new();
    myvector.extend(values.iter());

    // To see error, `&myvector` instead of `&input`
    gather_negatives(&input,
                     &mut myvector  );

    print!("myvector contains:");
    for it in &myvector {
        print!(" {}", *it);
    }
    println!("");
}
