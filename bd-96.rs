struct Foo { f: uint, g: uint }

fn main() {
    // `Box<T>` is a heap-allocated instance of T.
    let mut x: Box<Foo> = box Foo { f: 3, g: 4 };

    // `&mut T` is unique mutable reference to some T in memory.
    let y : &mut uint = &mut (*x).f;
    process(y);

    // Dangerous: mutation of x below frees old value, invalidating y.
    // Goal of borrow checker is to prevent such things.
    x = box Foo { f: 5, g: 6 };
}

fn process<T>(t: T) { }
