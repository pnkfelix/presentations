{
    // `Box<T>` is a heap-allocated instance of T.
    let mut x: Box<Foo> = box Foo { f: 3, name: "Foo_1" };
    println!("x is first: {}", x);

    // `&mut T` is unique mutable reference to some T in memory.
    {
        let y : &mut uint = &mut (*x).f;
        process(y);
    }

    // Dangerous: mutation of x below frees old value, invalidating y.
    // Goal of borrow checker is to prevent such things.
    x = box Foo { f: 5, name: "Foo_2" };
    println!("x is second: {}", x);
}
