struct Dollars { amt: int }
struct Euros { amt: int }
trait Currency {
    fn render(&self) -> ~str;
    fn to_euros(&self) -> Euros;
}
impl Currency for Dollars {
    fn render(&self) -> ~str { format!("${}", self.amt) }
    fn to_euros(&self) -> Euros { Euros { amt: (self.amt as f64 * 0.73) as int } }
}
impl Currency for Euros {
    fn render(&self) -> ~str { format!("€{}", self.amt) }
    fn to_euros(&self) -> Euros { *self }
}

fn add_as_euros<C:Currency>(a: &C, b: &C) -> Euros {
    Euros{ amt: a.to_euros().amt + b.to_euros().amt }
}

fn accumeuros(a: &Currency, b: &Currency) -> Euros {
    Euros{ amt: a.to_euros().amt + b.to_euros().amt }
}

fn eg_eu_eu() {
    let eu100 = Euros { amt: 100 };
    let eu200 = Euros { amt: 200 };
    println!("{:?}", add_as_euros(&eu100, &eu200));
}

fn eg_us_us() {
    let us100 = Dollars { amt: 100 };
    let us200 = Dollars { amt: 200 };
    println!("{:?}", add_as_euros(&us100, &us200));
}

#[cfg(us_eu)]
fn eg_us_eu() {
    let us100 = Dollars { amt: 100 };
    let eu200 = Euros { amt: 200 };
    println!("{:?}", add_as_euros(&us100, &eu200));
}

#[cfg(not(us_eu))]
fn eg_us_eu() {
    let us100 = Dollars { amt: 100 };
    let eu200 = Euros { amt: 200 };
    println!("{:?}", accumeuros(&us100 as &Currency,
                                &eu200 as &Currency));
}

#[cfg(not(us_eu))]
fn main_currency() {
    eg_us_us();
    eg_eu_eu();
    eg_us_eu();
}
#[cfg(us_eu)]
fn main_currency() {
    eg_us_eu();
}

struct Pair { x: int, y: int }
fn main_old2() {

let _p34 = Pair{ x: 3, y: 4 };

fn zero_x(p: Pair) -> Pair {
  return Pair{ x: 0, ..p };
}


    let p34 = Pair{ x: 3, y: 4 };
    let x_adjuster =
        |new_x| { Pair{ x: new_x, ..p34 } };

    let p14 = x_adjuster(1);
    let _p24 = x_adjuster(2);
    println!("p34: {:?} p14: {:?}", p34, p14);
}

fn foo() -> int { 3 }
fn bar() -> int { 4 }
fn quux() -> int { 5 }

fn main() {

    let p34 = Pair{ x: 3, y: 4 };

    fn zero_x(p: Pair) -> Pair { Pair{ x: 0, ..p } }

    println!("p34: {:?} zeroed p34: {:?}", p34, zero_x(p34));

let p34 = Pair{ x: 3, y: 4 };
let x_adjuster =
  |new_x| { Pair{ x: new_x, ..p34 } };

let p14 = x_adjuster(1);
let _p24 = x_adjuster(2);
println!("p34.x: {} p14.x: {}", p34.x, p14.x);

    let y = { let x = foo(); 2 + x };
    println!("y: {}", y);

    rest();
}

fn rest() {
    struct Pair { x: int, y: int }

    impl Pair {
        fn zeroed_x_copy(self) -> Pair {
            return Pair { x: 0, ..self }
        }

        fn replace_x(&mut self, new_x: int) {
            self.x = new_x;
        }
    }

    let mut p_tmp = Pair{ x: 5, y: 6 };
    let p06 = p_tmp.zeroed_x_copy();
    p_tmp.replace_x(17);
    println!("p_tmp: {:?} p06: {:?}", p_tmp, p06);
}
