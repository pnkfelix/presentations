use std::fmt;

fn main() {
    struct Foo { f: uint, name: &'static str }

    {
        include!("bd-96-concrete.rs")
    }

    impl Drop for Foo {
        fn drop(&mut self) { println!("dropping {}", self.name) }
    }

    impl fmt::Show for Foo {
        fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
            write!(w, "{:s}", self.name)
        }
    }

    fn process<T>(_t: T) { }
}
