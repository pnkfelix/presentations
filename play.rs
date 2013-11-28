fn foo() -> int { 3 }
fn bar() -> int { 4 }
fn quux() -> int { 5 }

fn main() {
    struct IntPair { x: int, y: int }

    let p34 = IntPair{ x: 3, y: 4 };

    fn zero_x(p: IntPair) -> IntPair { IntPair{ x: 0, ..p } }

    println!("p34: {:?} zeroed p34: {:?}", p34, zero_x(p34));

let p34 = IntPair{ x: 3, y: 4 };
let x_adjuster =
  |new_x| { IntPair{ x: new_x, ..p34 } };

let p14 = x_adjuster(1);
let p24 = x_adjuster(2);
println!("p34.x: {} p14.x: {}", p34.x, p14.x);

    let y = { let x = foo(); 2 + x };
    println!("y: {}", y);

    rest();
}

fn rest() {
    struct IntPair { x: int, y: int }

    impl IntPair {
        fn zeroed_x_copy(self) -> IntPair {
            return IntPair { x: 0, ..self }
        }

        fn replace_x(&mut self, new_x: int) {
            self.x = new_x;
        }
    }

    let mut p_tmp = IntPair{ x: 5, y: 6 };
    let p06 = p_tmp.zeroed_x_copy();
    p_tmp.replace_x(17);
    println!("p_tmp.x: {} p06.x: {}", p_tmp.x, p06.x);
}
