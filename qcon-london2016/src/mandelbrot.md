```rust
#![feature(augmented_assignments, op_assign_traits)]
#![allow(dead_code)]

extern crate piston_window;
extern crate image as im;
extern crate vecmath;
extern crate ramp;
extern crate num;
extern crate gmp;
extern crate stopwatch;

const REDUCE_VIA_LCM: bool = false;

use im::GenericImage;
use piston_window::*;
// use im::Pixel;
use stopwatch::Stopwatch;

use std::cell::Cell;
use std::cmp;
use std::thread;
use std::sync::mpsc::channel;

fn main() {
    let opengl = OpenGL::V3_2;
    let (mut width, mut height) = (300, 300);
    let window: PistonWindow =
        WindowSettings::new("piston: mandelbrot", (width, height))
        .exit_on_esc(true)
        .opengl(opengl)
        .build()
        .unwrap();

    let mut canvas = im::ImageBuffer::new(width, height);
    let mut texture = Texture::from_image(&mut *window.factory.borrow_mut(),
                                          &canvas,
                                          &TextureSettings::new()).unwrap();

    #[derive(Copy, Clone, PartialEq, Debug)]
    enum Mode {
        NextDrawRect,
        DrawingRect([f64; 2]),
        ZoomTo([f64; 2], [f64; 2]),
        Waiting
    };
    let mut mode = Mode::Waiting;

    #[derive(Clone, Debug)]
    struct DrawSpec {
        scale: Scale,
        width: u32,
        height: u32,
    }

    type IB = im::ImageBuffer<im::Rgba<u8>, Vec<u8>>;

    let orig_scale = Scale {
        x: [Frac::from(-5) / Frac::from(2),  Frac::from(1)],
        y: [Frac::from(-1), Frac::from(1)],
        width: width,
        height: height
        // x: [0.0, 1.0], y: [0.0, 1.0], width: width, height: height
    };

    let mut scale = {
        // let zoomed_scale = Scale {
        //     // x: [Frac::from(-2.5), Frac::from(1)],
        //     x: [Frac::from(-0.7387733983830036),  Frac::from(-0.7387733983830016)],
        //     y: [Frac::from(-0.13407787050277506), Frac::from(-0.13407787050277412)],
        //     width: width,
        //     height: height
        //     // x: [0.0, 1.0], y: [0.0, 1.0], width: width, height: height
        // };
        orig_scale.clone()
        // zoomed_scale
    };

    #[derive(Copy, Clone, Debug)]
    enum BgElem { Unknown, InSet, Escapes(u32), }

    let background_state: Vec<Cell<BgElem>> =
        vec![Cell::new(BgElem::Unknown); (width * height) as usize];
    const NUM_THREADS: u32 = 1;
    let mut handles_and_ports = vec![];
    let (tx, rx) = channel();

    for i in 0..NUM_THREADS {
        let (tx2, rx2) = channel();
        let tx = tx.clone();
        let mut scale = scale.clone();
        let handle = thread::spawn(move || {
            let work_size = (height + NUM_THREADS - 1) / NUM_THREADS;
            'draw: loop {
                const MAX_ITERATION: u32 = 0x1_00_00_00;
                const ITER_INCR: u32 = 0x1_00;
                const ITER_FACTOR: u32 = 4;
                // const ITER_INCR: u32 = 0x1;

                let mut max_iters = 4;
                while max_iters < MAX_ITERATION {
                    println!("thread: {} max_iters: 0x{:<8x} = {}", i, max_iters, max_iters);
                    let start_y = i * work_size;
                    let limit_y = cmp::min(start_y + work_size, height);
                    let mut saw_content = false;

                    let sw = Stopwatch::start_new();
                    for y in start_y..limit_y {
                        for x in 0..width {
                            let bg_elem = {
                                let z = Complex(Frac::from(0), Frac::from(0));
                                match mandelbrot(z, x, y, scale.clone(), max_iters) {
                                    Some(iters) => {
                                        saw_content = true;
                                        BgElem::Escapes(iters)
                                    }
                                    None => BgElem::Unknown,
                                }
                            };
                            tx.send((x, y, bg_elem)).unwrap();
                            if let Ok(spec) = rx2.try_recv() {
                                let spec: DrawSpec = spec;
                                scale = spec.scale;
                                width = spec.width;
                                height = spec.height;
                                continue 'draw;
                            }
                        }
                    }

                    println!("thread: {} iterations: {} time: {:?}", i, max_iters, sw.elapsed());
                    if !saw_content {
                        max_iters *= ITER_FACTOR;
                    } else {
                        max_iters += ITER_INCR;
                    }
                }

                // if we finish, then just wait for a new command to come in.
                let spec: DrawSpec = rx2.recv().unwrap();
                scale = spec.scale;
                width = spec.width;
                height = spec.height;
            }
        });
        handles_and_ports.push((handle, tx2));
    }

    let process_results = || {
        loop {
            match rx.try_recv() {
                Ok((x, y, bg_elem)) => {
                    let idx = x * width + y;
                    background_state[idx as usize].set(bg_elem);
                }
                Err(_) => break,
            }
        }
    };

    let background = |canvas: &mut IB, _scale: Scale| {
        for x in 0..width {
            for y in 0..height {
                let idx = x * width + y;
                let color = iters_to_color(match background_state[idx as usize].get() {
                    BgElem::Unknown => None,
                    BgElem::InSet => None,
                    BgElem::Escapes(iters) => Some(iters),
                });
                canvas.put_pixel(x, y, color);
            }
        }
    };

    texture.update(&mut *window.factory.borrow_mut(), &canvas).unwrap();

    let redo_background = |spec: DrawSpec| {
        for c in &background_state { c.set(BgElem::Unknown); }
        for &(_, ref p) in &handles_and_ports { p.send(spec.clone()).unwrap(); }
    };

    let mut last_pos = None;
    for e in window {
        e.draw_2d(|c, g| {
            clear([1.0; 4], g);
            image(&texture, c.transform, g);
        });

        let args = e.mouse_cursor_args();
        if args.is_some() {
            last_pos = args;
        }
        if let Some(_) = e.idle_args() {
            process_results();
            background(&mut canvas, scale.clone());
            texture.update(&mut *e.factory.borrow_mut(), &canvas).unwrap();
        } else if let Some(s) = e.text_args() {
            match &s[..] {
                "r" | "z" => {
                    scale = if &s[..] == "z" {
                        Scale {
                            x: [Frac::from(-0.7387733983830036),  Frac::from(-0.7387733983830016)],
                            y: [Frac::from(-0.13407787050277506), Frac::from(-0.13407787050277412)],
                            width: width,
                            height: height
                        }
                    } else {
                        orig_scale.clone()
                    };
                    let spec = DrawSpec {
                        scale: scale.clone(),
                        width: width,
                        height: height,
                    };
                    println!("resetting to spec: {:?} under mode: {:?}", spec, mode);
                    redo_background(spec);
                }
                "-" => {
                    let x0 = scale.x[0].clone();
                    let x1 = scale.x[1].clone();
                    let y0 = scale.y[0].clone();
                    let y1 = scale.y[1].clone();
                    let w_2 = (x1.clone() - x0.clone()) / 2;
                    let h_2 = (y1.clone() - y0.clone()) / 2;
                    scale = Scale {
                        x: [x0 - w_2.clone(), x1 + w_2],
                        y: [y0 - h_2.clone(), y1 + h_2],
                        ..scale
                    };
                    redo_background(DrawSpec {
                        scale: scale.clone(),
                        width: width,
                        height: height,
                    })
                }
                "+" => {
                    let x0 = scale.x[0].clone();
                    let x1 = scale.x[1].clone();
                    let y0 = scale.y[0].clone();
                    let y1 = scale.y[1].clone();
                    let w_4 = (x1.clone() - x0.clone()) / 4;
                    let h_4 = (y1.clone() - y0.clone()) / 4;
                    scale = Scale {
                        x: [x0 + w_4.clone(), x1 - w_4.clone()],
                        y: [y0 + h_4.clone(), y1 - h_4.clone()],
                        ..scale
                    };
                    redo_background(DrawSpec {
                        scale: scale.clone(),
                        width: width,
                        height: height,
                    })
                }
                _ => {}
            }
        } else if let Some(_button) = e.press_args() {
            mode = if let Some(pos) = args {
                Mode::DrawingRect(pos)
            } else {
                Mode::NextDrawRect
            };
        } else if let Some(_button) = e.release_args() {
            if let (Mode::DrawingRect(last), Some(last_pos)) = (mode, last_pos) {
                mode = Mode::ZoomTo(last, last_pos);
            } else {
                mode = Mode::Waiting;
            }
        } else if let (Mode::NextDrawRect, Some(pos)) = (mode, args) {
            mode = Mode::DrawingRect(pos);
        } else if let Some(resize) = e.resize_args() {
            println!("window resized: {:?}", resize);
        }

        if let Mode::ZoomTo(p1, p2) = mode {
            println!("zooming to mode: {:?}", mode);
            scale.zoom_to(p1, p2);
            redo_background(DrawSpec { scale: scale.clone(), width: width, height: height, });
            mode = Mode::Waiting;
        }

        let in_range = |curr: [f64; 2]| {
            let x = curr[0]; let y = curr[1];
            0f64 <= x && x < (width as f64) && 0f64 <= y && y < (height as f64)
        };

        if let (Mode::DrawingRect(start), Some(curr)) = (mode, last_pos) {
            if in_range(curr) { 
                let color = im::Rgba([0, 0, 0, 128]);

                let start = (start[0] as u32, start[1] as u32);
                let curr = (curr[0] as u32, curr[1] as u32);

                let (min_x, max_x) = minmax(start.0, curr.0);
                let (min_y, max_y) = minmax(start.1, curr.1);

                for x in cmp::max(0, min_x)..cmp::min(width, max_x) {
                    if min_y < height { canvas[(x, min_y)] = color; }
                    if max_y < height { canvas[(x, max_y)] = color; }
                }
                for y in cmp::max(0, min_y)..cmp::min(height, max_y) {
                    if min_x < width { canvas[(min_x, y)] = color; }
                    if max_x < width { canvas[(max_x, y)] = color; }
                }
            }
        }
        texture.update(&mut *e.factory.borrow_mut(), &canvas).unwrap();
    }
}

// use frac_type_f64::Frac;
use frac_wrap_f64::Frac;
// use frac_bigratio::Frac;
// use frac_dynamic::Frac;
// use frac_mpq::Frac;
// use frac_florat::Frac;
// use frac_fixrat::Frac;
// use frac_limit_bigratio::Frac;

mod frac_limit_bigratio {
    use std::cmp;
    use ramp;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Frac { numer: ramp::Int, denom: ramp::Int, }
    impl Frac { pub fn bit_length(&self) -> u32 { self.numer.bit_length() + self.denom.bit_length() } }
    impl Frac { pub fn divide_by_4(&mut self) { self.numer >>= 2 } }
    impl Frac {
        pub fn drop_bits(&mut self, num_bits: usize) {
            self.numer >>= num_bits;
            self.denom >>= num_bits;
        }
    }
    use std::ops::{Add, AddAssign, Sub, SubAssign, Mul, MulAssign, Div};

    impl Frac {
        fn reduce(self) -> Self {
            let bits = self.bit_length();
            if bits > 24 {
                println!("reduce self: {:?} bits: {}", self, bits);
            }
            self
        }
        pub fn sqr(self) -> Self {
            let Frac { numer, denom } = self;
            Frac { numer: numer.dsquare(), denom: denom.dsquare() }
        }
    }

    impl Add<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn add(mut self, other : Frac) -> Frac {
            let f = if self.denom == other .denom {
                self.numer += other .numer;
                self
            } else if ::REDUCE_VIA_LCM {
                // let denom = self.denom.clone() * other .denom.clone();
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= self.denom;
                let mut b = denom.clone(); b /= other .denom;

                self.numer *= a;
                self.numer += other .numer * b;
                self.denom = denom;
                self
            } else {
                Frac { numer: self.numer * other .denom.clone() + other .numer * self.denom.clone(),
                       denom: self.denom * other .denom }
            };
            f.reduce()
        }
    }
    impl Add<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn add(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer + self.denom.clone() * (other  as usize),
                           denom: self.denom };
            f.reduce()
        }
    }

    impl<'a> AddAssign<&'a Frac> for Frac {
        #[inline]
        fn add_assign(&mut self, other : &'a Frac) {
            if self.denom == other .denom {
                self.numer += other .numer.clone();
            } else if ::REDUCE_VIA_LCM {
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= self.denom.clone();
                let mut b = denom.clone(); b /= other .denom.clone();

                self.numer *= a;
                self.numer += other .numer.clone() * b;
                self.denom = denom;
            } else {
                self.numer *= &other .denom;
                self.numer += other .numer.clone() * &self.denom;
                self.denom *= &other .denom;
            }
        }
    }
    impl Sub<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn sub(mut self, other : Frac) -> Frac {
            let f = if self.denom == other .denom {
                self.numer -= other .numer;
                self
            } else if ::REDUCE_VIA_LCM {
                // let denom = self.denom.clone() * other .denom.clone();
                let denom = self.denom.lcm(&other .denom);
                let a = denom.clone() / self.denom;
                let b = denom.clone() / other .denom;
                self.numer *= a;
                self.numer -= other .numer * b;
                self.denom = denom;
                self
            } else {
                Frac { numer: self.numer * other .denom.clone() - other .numer * self.denom.clone(),
                       denom: self.denom * other .denom }
            };
            f.reduce()
        }
    }
    impl Sub<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn sub(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer - self.denom.clone() * (other  as usize),
                           denom: self.denom };
            f.reduce()
        }
    }
    impl<'a> SubAssign<&'a Frac> for Frac {
        #[inline]
        fn sub_assign(&mut self, other : &'a Frac) {
            if self.denom == other .denom {
                self.numer -= other .numer.clone();
            } else if ::REDUCE_VIA_LCM {
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= &self.denom;
                let mut b = denom.clone(); b /= &other .denom;

                self.numer *= a;
                self.numer -= &other .numer * b;
                self.denom = denom;
            } else {
                self.numer *= &other .denom;
                self.numer -= other .numer.clone() * &self.denom;
                self.denom *= &other .denom;
            }
        }
    }
    impl Mul<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn mul(self, other : Frac) -> Frac {
            let f = Frac { numer: self.numer * other .numer, denom: self.denom * other .denom };
            f.reduce()
        }
    }
    impl Mul<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn mul(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer * (other  as usize), denom: self.denom };
            f.reduce()
        }
    }
    impl<'a> MulAssign<&'a Frac> for Frac {
        #[inline]
        fn mul_assign(&mut self, other : &'a Frac) {
            self.numer *= other .numer.clone();
            self.denom *= other .denom.clone();
        }
    }
    impl Div<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn div(self, other : Frac) -> Frac {
            let f = Frac { numer: self.numer * other .denom, denom: self.denom * other .numer };
            f.reduce()
        }
    }
    impl Div<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn div(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer, denom: self.denom * (other  as usize) };
            f.reduce()
        }
    }

    impl PartialOrd for Frac {
        #[inline]
        fn partial_cmp(&self, other : &Frac) -> Option<cmp::Ordering> {
            let lhs = self.numer.clone() * other .denom.clone();
            let rhs = self.denom.clone() * other .numer.clone();
            lhs.partial_cmp(&rhs)
        }
    }

    impl From<u32> for Frac {
        #[inline]
        fn from(n: u32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<i32> for Frac {
        #[inline]
        fn from(n: i32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<i64> for Frac {
        #[inline]
        fn from(n: i64) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
}

mod frac_fixrat {
    use std::cmp;
    use std::ops;
    #[derive(Copy, Clone, PartialEq, Debug)]
    pub struct Frac { numer: i64, denom: i64 }

    use num::integer::Integer;

    impl Frac {
        pub fn bit_length(&self) -> u32 { 128 }
        pub fn sqr(&self) -> Frac { *self * *self }
    }

    impl Frac {
        #[inline]
        fn reduce(self) -> Frac {
            use num::integer::Integer;
            let Frac { numer, denom } = self;
            let g = numer.gcd(&denom);
            if g != 0 {
                Frac { numer: numer / g, denom: denom / g }
            } else {
                Frac { numer: numer, denom: denom }
            }
        }

        #[inline]
        fn drop_bits(&mut self, num_bits: usize) {
            self.numer >>= num_bits;
            self.denom >>= num_bits;
        }
    }

    impl From<i32> for Frac { fn from(x: i32) -> Frac { Frac { numer: x as i64, denom: 1 } } }
    impl From<i64> for Frac { fn from(x: i64) -> Frac { Frac { numer: x, denom: 1 } } }
    impl From<u32> for Frac { fn from(x: u32) -> Frac { Frac { numer: x as i64, denom: 1 } } }

    impl cmp::PartialOrd for Frac {
        fn partial_cmp(&self, other: &Frac) -> Option<cmp::Ordering> {
            // (self.numer / self.denom).partial_cmp(&(other.numer / other.denom))
            (self.numer as f64 * other.denom as f64).partial_cmp(&(other.numer as f64 * self.denom as f64))
        }
    }

    impl ops::Add<Frac> for Frac {
        type Output = Frac;
        fn add(mut self, mut other: Frac) -> Frac {
            match (self.numer.overflowing_mul(other.denom),
                   other.numer.overflowing_mul(self.denom),
                   self.denom.overflowing_mul(other.denom)) {
                ((n1, false), (n2, false), (d, false)) => {
                    if let (sum, false) = n1.overflowing_add(n2) {
                        return Frac { numer: sum, denom: d };
                    }
                }
                _ => {}
            }
            self.drop_bits(32);
            other.drop_bits(32);
            (Frac { numer: self.numer * other.denom + other.numer*self.denom,
                    denom: self.denom * other.denom }).reduce()
        }
    }
    impl ops::Sub<Frac> for Frac {
        type Output = Frac;
        fn sub(mut self, mut other : Frac) -> Frac {
            match (self.numer.overflowing_mul(other.denom),
                   other.numer.overflowing_mul(self.denom),
                   self.denom.overflowing_mul(other.denom)) {
                ((n1, false), (n2, false), (d, false)) => return Frac { numer: n1 - n2, denom: d },
                _ => {}
            }
            self.drop_bits(32);
            other.drop_bits(32);
            (Frac { numer: self.numer * other.denom - other.numer*self.denom,
                    denom: self.denom * other.denom }).reduce()
        }
    }
    impl ops::Mul<Frac> for Frac {
        type Output = Frac;
        fn mul(mut self, mut other: Frac) -> Frac {
            match (self.numer.overflowing_mul(other.numer),
                   self.denom.overflowing_mul(other.denom)) {
                ((n, false), (d, false)) => return Frac { numer: n, denom: d },
                _ => {}
            }
            self.drop_bits(33);
            other.drop_bits(33);
            (Frac { numer: self.numer * other.numer,
                    denom: self.denom * other.denom }).reduce()
        }
    }
    impl ops::Div<Frac> for Frac {
        type Output = Frac;
        fn div(self, other : Frac) -> Frac {
            (Frac { numer: self.numer * other.denom,
                    denom: self.denom * other.numer }).reduce()
        }
    }

    impl ops::AddAssign<Frac> for Frac {
        fn add_assign(&mut self, other: Frac) { *self = *self + other  }
    }
    impl<'a> ops::AddAssign<&'a Frac> for Frac {
        fn add_assign(&mut self, other: &'a Frac) { *self = *self + *other  }
    }
    impl ops::SubAssign<Frac> for Frac {
        fn sub_assign(&mut self, other: Frac) { *self = *self - other  }
    }
    impl<'a> ops::SubAssign<&'a Frac> for Frac {
        fn sub_assign(&mut self, other: &'a Frac) { *self = *self - *other  }
    }
    impl ops::MulAssign<Frac> for Frac {
        fn mul_assign(&mut self, other: Frac) { *self = *self * other  }
    }
    impl<'a> ops::MulAssign<&'a Frac> for Frac {
        fn mul_assign(&mut self, other: &'a Frac) { *self = *self * *other  }
    }
    impl ops::DivAssign<Frac> for Frac {
        fn div_assign(&mut self, other: Frac) { *self = *self / other  }
    }
    impl<'a> ops::DivAssign<&'a Frac> for Frac {
        fn div_assign(&mut self, other: &'a Frac) { *self = *self / *other  }
    }

    impl<'a> ops::Add<&'a Frac> for Frac {
        type Output = Frac;
        fn add(self, other: &'a Frac) -> Frac { self + *other }
    }

    impl ops::Div<i32> for Frac {
        type Output = Frac;
        fn div(self, other: i32) -> Frac { self / Frac::from(other) }
    }
}

mod frac_florat {
    use std::cmp;
    use std::ops;
    #[derive(Copy, Clone, PartialEq, Debug)]
    pub struct Frac { numer: f64, denom: f64 }

    impl Frac {
        pub fn bit_length(&self) -> u32 { 128 }
        pub fn sqr(&self) -> Frac { *self * *self }
    }

    impl Frac {
        #[inline]
        fn reduce(self) -> Frac {
            let Frac { mut numer, mut denom } = self;

            let mut _saw_count = false;
            let mut counts = [0; 9];
            for &(i, red) in &[(1, (1024.0*1024.0*1024.0*1024.0*
                                    1024.0*1024.0*1024.0*1024.0*
                                    1024.0*1024.0*1024.0*1024.0*
                                    1024.0*1024.0*1024.0*1024.0)),
                               (2, (1024.0*1024.0*1024.0*1024.0*
                                    1024.0*1024.0*1024.0*1024.0)),
                               (3, 1024.0*1024.0*1024.0*1024.0),
                               (4, 1024.0*1024.0),
                               (5, 1024.0),
                               (6, 128.0),
                               (7, 16.0),
                               (8, 2.0)]
            {
                while numer % red == 0.0 && denom % red == 0.0 {
                    numer /= red;
                    denom /= red;
                    counts[i] += 1;
                    _saw_count = true;
                }
            }
            if counts[1] > 0 || counts[2] > 1 {
                println!("counts: {:?}", &counts[1..]);
            }
            Frac { numer: numer, denom: denom }
        }
    }

    impl From<i32> for Frac { fn from(x: i32) -> Frac { Frac { numer: x as f64, denom: 1.0 } } }
    impl From<i64> for Frac { fn from(x: i64) -> Frac { Frac { numer: x as f64, denom: 1.0 } } }
    impl From<u32> for Frac { fn from(x: u32) -> Frac { Frac { numer: x as f64, denom: 1.0 } } }

    impl cmp::PartialOrd for Frac {
        fn partial_cmp(&self, other: &Frac) -> Option<cmp::Ordering> {
            // (self.numer / self.denom).partial_cmp(&(other.numer / other.denom))
            (self.numer * other.denom).partial_cmp(&(other.numer * self.denom))
        }
    }

    impl ops::Add<Frac> for Frac {
        type Output = Frac;
        fn add(self, other : Frac) -> Frac {
            (Frac { numer: self.numer * other.denom + other.numer*self.denom,
                    denom: self.denom * other.denom }).reduce()
        }
    }
    impl ops::Sub<Frac> for Frac {
        type Output = Frac;
        fn sub(self, other : Frac) -> Frac {
            (Frac { numer: self.numer * other.denom - other.numer*self.denom,
                    denom: self.denom * other.denom }).reduce()
        }
    }
    impl ops::Mul<Frac> for Frac {
        type Output = Frac;
        fn mul(self, other : Frac) -> Frac {
            (Frac { numer: self.numer * other.numer,
                    denom: self.denom * other.denom }).reduce()
        }
    }
    impl ops::Div<Frac> for Frac {
        type Output = Frac;
        fn div(self, other : Frac) -> Frac {
            (Frac { numer: self.numer * other.denom,
                    denom: self.denom * other.numer }).reduce()
        }
    }

    impl ops::AddAssign<Frac> for Frac {
        fn add_assign(&mut self, other: Frac) { *self = *self + other  }
    }
    impl<'a> ops::AddAssign<&'a Frac> for Frac {
        fn add_assign(&mut self, other: &'a Frac) { *self = *self + *other  }
    }
    impl ops::SubAssign<Frac> for Frac {
        fn sub_assign(&mut self, other: Frac) { *self = *self - other  }
    }
    impl<'a> ops::SubAssign<&'a Frac> for Frac {
        fn sub_assign(&mut self, other: &'a Frac) { *self = *self - *other  }
    }
    impl ops::MulAssign<Frac> for Frac {
        fn mul_assign(&mut self, other: Frac) { *self = *self * other  }
    }
    impl<'a> ops::MulAssign<&'a Frac> for Frac {
        fn mul_assign(&mut self, other: &'a Frac) { *self = *self * *other  }
    }
    impl ops::DivAssign<Frac> for Frac {
        fn div_assign(&mut self, other: Frac) { *self = *self / other  }
    }
    impl<'a> ops::DivAssign<&'a Frac> for Frac {
        fn div_assign(&mut self, other: &'a Frac) { *self = *self / *other  }
    }

    impl<'a> ops::Add<&'a Frac> for Frac {
        type Output = Frac;
        fn add(self, other: &'a Frac) -> Frac { self + *other }
    }

    impl ops::Div<i32> for Frac {
        type Output = Frac;
        fn div(self, other: i32) -> Frac { self / Frac::from(other) }
    }
}

mod frac_mpq {
    use gmp::mpq::Mpq;
    use std::cmp;
    use std::ops;
    #[derive(Clone, Debug)]
    pub struct Frac(Mpq);
    impl Frac {
        pub fn bit_length(&self) -> u32 { 0 }
        #[cfg(not_now)]
        pub fn bit_length(&self) -> u32 {
            use std::mem;
            use gmp::mpz::Mpz;
            #[repr(C)]
            struct mpq_struct {
                _mp_num: Mpz,
                _mp_den: Mpz,
            }

            let self_: &mpq_struct = unsafe { mem::transmute(self) };
            (self_._mp_num.bit_length() + self_._mp_den.bit_length()) as u32
        }
        pub fn sqr(&self) -> Self {
            self.clone() * self.clone()
        }
    }

    impl From<u32> for Frac { fn from(x: u32) -> Frac { Frac(From::from(x as i64)) } }
    impl From<i32> for Frac { fn from(x: i32) -> Frac { Frac(From::from(x as i64)) } }
    impl From<i64> for Frac { fn from(x: i64) -> Frac { Frac(From::from(x)) } }

    impl cmp::PartialEq for Frac {
        fn eq(&self, other : &Frac) -> bool {
            self.0.eq(&other .0)
        }
    }
    impl cmp::PartialOrd for Frac {
        fn partial_cmp(&self, other : &Frac) -> Option<cmp::Ordering> {
            self.0.partial_cmp(&other .0)
        }
    }

    impl ops::Add<Frac> for Frac {
        type Output = Frac;
        fn add(self, other : Frac) -> Frac { Frac(self.0 + other .0) }
    }
    impl ops::AddAssign<Frac> for Frac {
        fn add_assign(&mut self, other : Frac) { *self = self.clone() + other ; }
    }
    impl<'a> ops::AddAssign<&'a Frac> for Frac {
        fn add_assign(&mut self, other : &'a Frac) { *self = self.clone() + other .clone(); }
    }
    impl ops::Sub<Frac> for Frac {
        type Output = Frac;
        fn sub(self, other : Frac) -> Frac { Frac(self.0 - other .0) }
    }
    impl ops::SubAssign<Frac> for Frac {
        fn sub_assign(&mut self, other : Frac) { *self = self.clone() - other ; }
    }
    impl<'a> ops::SubAssign<&'a Frac> for Frac {
        fn sub_assign(&mut self, other : &'a Frac) { *self = self.clone() - other .clone(); }
    }
    impl ops::Mul<Frac> for Frac {
        type Output = Frac;
        fn mul(self, other : Frac) -> Frac { Frac(self.0 * other .0) }
    }
    impl ops::MulAssign<Frac> for Frac {
        fn mul_assign(&mut self, other : Frac) { *self = self.clone() * other ; }
    }
    impl<'a> ops::MulAssign<&'a Frac> for Frac {
        fn mul_assign(&mut self, other : &'a Frac) { *self = self.clone() * other .clone(); }
    }
    impl ops::Div<Frac> for Frac {
        type Output = Frac;
        fn div(self, other : Frac) -> Frac { Frac(self.0 / other .0) }
    }
    impl ops::DivAssign<Frac> for Frac {
        fn div_assign(&mut self, other : Frac) { *self = self.clone() / other ; }
    }
    impl<'a> ops::DivAssign<&'a Frac> for Frac {
        fn div_assign(&mut self, other : &'a Frac) { *self = self.clone() / other .clone(); }
    }
    impl ops::Div<i32> for Frac {
        type Output = Frac;
        fn div(self, other : i32) -> Frac { Frac(self.0 / Mpq::from(other  as i64)) }
    }
}

mod frac_dynamic {
    use num::integer::Integer;
    use ramp::Int;
    use std::cmp;
    use std::ops;
    use self::MidInt::{A, B};

    #[derive(Clone, Debug, PartialEq)]
    enum MidInt { A(i64), B(Int), }

    impl MidInt {
        fn bit_length(&self) -> u32 {
            match *self {
                A(_) => 64,
                B(ref i) => i.bit_length(),
            }
        }

        fn gcd(&self, other : &MidInt) -> MidInt {
            match (self, other ) {
                (&A(ref s), &A(ref o)) => A(s.gcd(&o)),
                (&B(ref s), &B(ref o)) => B(s.gcd(&o)),
                (&A(ref s), &B(ref o)) => B(Int::from(*s).gcd(&o)),
                (&B(ref s), &A(ref o)) => B(s.gcd(&Int::from(*o))),
            }
        }

        fn dsquare(self) -> MidInt {
            match self {
                B(ref s) => B(s * s),
                A(s) => match s.overflowing_mul(s) {
                    (prod, false) => MidInt::A(prod),
                    (_, true) => MidInt::B(Int::from(s) * Int::from(s)),
                },
            }
        }

        fn lcm(&self, other : &MidInt) -> MidInt {
            match (self, other ) {
                (&A(ref s), &A(ref o)) => A(super::lcm(*s, *o)),
                (&B(ref s), &B(ref o)) => B(s.lcm(&o)),
                (&A(ref s), &B(ref o)) => B(Int::from(*s).lcm(&o)),
                (&B(ref s), &A(ref o)) => B(s.lcm(&Int::from(*o))),
            }
        }
    }

    impl From<i64> for MidInt {
        fn from(x: i64) -> MidInt { A(x) }
    }

    impl From<u32> for MidInt {
        fn from(x: u32) -> MidInt { A(x as i64) }
    }

    impl From<i32> for MidInt {
        fn from(x: i32) -> MidInt { A(x as i64) }
    }

    impl cmp::PartialOrd for MidInt {
        fn partial_cmp(&self, other : &MidInt) -> Option<cmp::Ordering> {
            match (self, other ) {
                (&A(ref s), &A(ref o)) => s.partial_cmp(o),
                (&B(ref s), &B(ref o)) => s.partial_cmp(o),
                (&A(ref s), &B(ref o)) => Int::from(*s).partial_cmp(o),
                (&B(ref s), &A(ref o)) => s.partial_cmp(&Int::from(*o)),
            }
        }
    }

    impl ops::ShrAssign<usize> for MidInt {
        fn shr_assign(&mut self, o: usize) {
            *self = match *self {
                A(ref s) => A(s >> o),
                B(ref s) => B(s >> o),
            };
        }
    }

    impl ops::Add<MidInt> for MidInt {
        type Output = MidInt;
        fn add(self, o: MidInt) -> MidInt {
            match (self, o) {
                (A(s), A(o)) => match s.overflowing_add(o) {
                    (sum, false) => A(sum),
                    (_, true) => B(Int::from(s) + Int::from(o)),
                },
                (B(s), B(o)) => B(s + o),
                (A(s), B(o)) => B(Int::from(s) + o),
                (B(s), A(o)) => B(s + Int::from(o)),
            }
        }
    }

    impl ops::Sub<MidInt> for MidInt {
        type Output = MidInt;
        fn sub(self, o: MidInt) -> MidInt {
            match (self, o) {
                (A(s), A(o)) => match s.overflowing_sub(o) {
                    (sum, false) => A(sum),
                    (_, true) => B(Int::from(s) - Int::from(o)),
                },
                (B(s), B(o)) => B(s - o),
                (A(s), B(o)) => B(Int::from(s) - o),
                (B(s), A(o)) => B(s - Int::from(o)),
            }
        }
    }

    impl ops::AddAssign<MidInt> for MidInt {
        fn add_assign(&mut self, other : MidInt) {
            use std::ptr;
            let self_ = self as *mut _;
            let sum = match (self, other ) {
                (&mut A(s), A(o)) => match s.overflowing_add(o) {
                    (sum, false) => { A(sum) }
                    (_, true) => { B(Int::from(s) + Int::from(o)) }
                },
                (&mut B(ref mut s), B(ref o)) => { *s += o; return; }
                (&mut A(s), B(o)) => { B(Int::from(s) + o) },
                (&mut B(ref mut s), A(o)) => { *s += Int::from(o); return; }
            };
            unsafe { ptr::write(self_, sum); }
        }
    }

    impl ops::SubAssign<MidInt> for MidInt {
        fn sub_assign(&mut self, other : MidInt) {
            use std::ptr;
            let self_ = self as *mut _;
            let sum = match (self, other ) {
                (&mut A(s), A(o)) => match s.overflowing_sub(o) {
                    (sum, false) => { A(sum) }
                    (_, true) => { B(Int::from(s) - Int::from(o)) }
                },
                (&mut B(ref mut s), B(ref o)) => { *s -= o; return; }
                (&mut A(s), B(o)) => { B(Int::from(s) - o) },
                (&mut B(ref mut s), A(o)) => { *s -= Int::from(o); return; }
            };
            unsafe { ptr::write(self_, sum); }
        }
    }

    impl ops::Mul<MidInt> for MidInt {
        type Output = MidInt;
        fn mul(self, o: MidInt) -> MidInt {
            match (self, o) {
                (A(s), A(o)) => match s.overflowing_mul(o) {
                    (prod, false) => MidInt::A(prod),
                    (_, true) => MidInt::B(Int::from(s) * Int::from(o)),
                },
                (B(s), B(o)) => B(s * o),
                (A(s), B(o)) => B(Int::from(s) * o),
                (B(s), A(o)) => B(s * Int::from(o)),
            }
        }
    }

    impl<'a> ops::Mul<&'a MidInt> for MidInt {
        type Output = MidInt;
        fn mul(self, o: &'a MidInt) -> MidInt {
            match (self, o) {
                (A(s), &A(ref o)) => match s.overflowing_mul(*o) {
                    (prod, false) => MidInt::A(prod),
                    (_, true) => MidInt::B(Int::from(s) * Int::from(*o)),
                },
                (B(ref s), &B(ref o)) => B(s * o),
                (A(s), &B(ref o)) => B(Int::from(s) * o),
                (B(ref s), &A(ref o)) => B(s * Int::from(*o)),
            }
        }
    }

    impl<'b> ops::Mul<MidInt> for &'b MidInt {
        type Output = MidInt;
        fn mul(self, o: MidInt) -> MidInt {
            match (self, o) {
                (&A(ref s), A(ref o)) => match s.overflowing_mul(*o) {
                    (prod, false) => MidInt::A(prod),
                    (_, true) => MidInt::B(Int::from(*s) * Int::from(*o)),
                },
                (&B(ref s), B(ref o)) => B(s * o),
                (&A(ref s), B(ref o)) => B(Int::from(*s) * o),
                (&B(ref s), A(ref o)) => B(s * Int::from(*o)),
            }
        }
    }

    impl<'a, 'b> ops::Mul<&'a MidInt> for &'b MidInt {
        type Output = MidInt;
        fn mul(self, o: &'a MidInt) -> MidInt {
            match (self, o) {
                (&A(ref s), &A(ref o)) => match s.overflowing_mul(*o) {
                    (prod, false) => MidInt::A(prod),
                    (_, true) => MidInt::B(Int::from(*s) * Int::from(*o)),
                },
                (&B(ref s), &B(ref o)) => B(s * o),
                (&A(ref s), &B(ref o)) => B(Int::from(*s) * o),
                (&B(ref s), &A(ref o)) => B(s * Int::from(*o)),
            }
        }
    }

    impl ops::Mul<u32> for MidInt {
        type Output = MidInt;
        fn mul(self, o: u32) -> MidInt {
            match self {
                A(s) => match s.overflowing_mul(o as i64) {
                    (prod, false) => A(prod),
                    (_, true) => B(Int::from(s) * Int::from(o))
                },
                B(s) => B(s * Int::from(o)),
            }
        }
    }

    impl ops::MulAssign<MidInt> for MidInt {
        fn mul_assign(&mut self, o: MidInt) {
            let self_ = self as *mut _;
            let prod = match (self, o) {
                (&mut A(ref mut s), A(o)) => match s.overflowing_mul(o) {
                    (prod, false) => { *s = prod; return; }
                    (_, true) => MidInt::B(Int::from(*s) * Int::from(o)),
                },
                (&mut B(ref mut s), B(ref o)) => { *s *= o; return; }
                (&mut A(s), B(o)) => { B(Int::from(s) * o) }
                (&mut B(ref mut s), A(o)) => { *s *= Int::from(o); return; }
            };
            unsafe { *self_ = prod; }
        }
    }

    impl<'a> ops::MulAssign<&'a MidInt> for MidInt {
        fn mul_assign(&mut self, o: &'a MidInt) {
            use std::ptr;
            let self_ = self as *mut _;
            let prod = match (self, o) {
                (&mut A(ref mut s), &A(o)) => match s.overflowing_mul(o) {
                    (prod, false) => MidInt::A(prod),
                    (_, true) => { MidInt::B(Int::from(*s) * Int::from(o)) }
                },
                (&mut B(ref mut s), &B(ref o)) => { *s *= o; return; }
                (&mut A(s), &B(ref o)) => { B(Int::from(s) * o) }
                (&mut B(ref mut s), &A(o)) => { *s *= Int::from(o); return; }
            };
            unsafe { ptr::write(self_, prod); }
        }
    }

    impl ops::Div<MidInt> for MidInt {
        type Output = MidInt;
        fn div(self, o: MidInt) -> MidInt {
            match (self, o) {
                (A(s), A(o)) => A(s / o),
                (B(s), B(o)) => B(s / o),
                (A(s), B(o)) => B(Int::from(s) / o),
                (B(s), A(o)) => B(s / Int::from(o)),
            }
        }
    }

    impl ops::DivAssign<MidInt> for MidInt {
        fn div_assign(&mut self, o: MidInt) {
            use std::ptr;
            let self_ = self as *mut _;
            let prod = match (self, o) {
                (&mut A(ref mut s), A(o)) => { *s /= o; return; }
                (&mut B(ref mut s), B(ref o)) => { *s /= o; return; }
                (&mut A(s), B(ref o)) => { B(Int::from(s) / o) }
                (&mut B(ref mut s), A(o)) => { *s /= Int::from(o); return; }
            };
            unsafe { ptr::write(self_, prod); }
        }
    }

    impl<'a> ops::DivAssign<&'a MidInt> for MidInt {
        fn div_assign(&mut self, o: &'a MidInt) {
            use std::ptr;
            let self_ = self as *mut _;
            let prod = match (self, o) {
                (&mut A(ref mut s), &A(o)) => { *s /= o; return; }
                (&mut B(ref mut s), &B(ref o)) => { *s /= o; return; }
                (&mut A(s), &B(ref o)) => { B(Int::from(s) / o) }
                (&mut B(ref mut s), &A(o)) => { *s /= Int::from(o); return; }
            };
            unsafe { ptr::write(self_, prod); }
        }
    }

    #[derive(Clone, Debug, PartialEq)]
    pub struct Frac { numer: MidInt, denom: MidInt, }
    impl Frac { pub fn bit_length(&self) -> u32 { self.numer.bit_length() + self.denom.bit_length() } }
    impl Frac { pub fn divide_by_4(&mut self) { self.numer >>= 2 } }
    use std::ops::{Add, AddAssign, Sub, SubAssign, Mul, MulAssign, Div};

    impl Frac {
        fn reduce(self) -> Self {
            let Frac { numer, denom } = self;
            let gcd = numer.gcd(&denom);
            Frac { numer: numer / gcd.clone(), denom: denom / gcd }
            // self
        }
        pub fn sqr(self) -> Self {
            let Frac { numer, denom } = self;
            Frac { numer: numer.dsquare(), denom: denom.dsquare() }
        }
    }

    impl Add<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn add(mut self, other : Frac) -> Frac {
            let f = if self.denom == other .denom {
                self.numer += other .numer;
                self
            } else if ::REDUCE_VIA_LCM {
                // let denom = self.denom.clone() * other .denom.clone();
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= self.denom;
                let mut b = denom.clone(); b /= other .denom;

                self.numer *= a;
                self.numer += other .numer * b;
                self.denom = denom;
                self
            } else {
                Frac { numer: self.numer * other .denom.clone() + other .numer * self.denom.clone(),
                       denom: self.denom * other .denom }
            };
            f.reduce()
        }
    }
    impl Add<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn add(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer + self.denom.clone() * other ,
                           denom: self.denom };
            f.reduce()
        }
    }

    impl<'a> AddAssign<&'a Frac> for Frac {
        #[inline]
        fn add_assign(&mut self, other : &'a Frac) {
            if self.denom == other .denom {
                self.numer += other .numer.clone();
            } else if ::REDUCE_VIA_LCM {
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= self.denom.clone();
                let mut b = denom.clone(); b /= other .denom.clone();

                self.numer *= a;
                self.numer += other .numer.clone() * b;
                self.denom = denom;
            } else {
                self.numer *= &other .denom;
                self.numer += other .numer.clone() * &self.denom;
                self.denom *= &other .denom;
            }
        }
    }
    impl Sub<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn sub(mut self, other : Frac) -> Frac {
            let f = if self.denom == other .denom {
                self.numer -= other .numer;
                self
            } else if ::REDUCE_VIA_LCM {
                // let denom = self.denom.clone() * other .denom.clone();
                let denom = self.denom.lcm(&other .denom);
                let a = denom.clone() / self.denom;
                let b = denom.clone() / other .denom;
                self.numer *= a;
                self.numer -= other .numer * b;
                self.denom = denom;
                self
            } else {
                Frac { numer: self.numer * other .denom.clone() - other .numer * self.denom.clone(),
                       denom: self.denom * other .denom }
            };
            f.reduce()
        }
    }
    impl Sub<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn sub(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer - self.denom.clone() * other ,
                           denom: self.denom };
            f.reduce()
        }
    }
    impl<'a> SubAssign<&'a Frac> for Frac {
        #[inline]
        fn sub_assign(&mut self, other : &'a Frac) {
            if self.denom == other .denom {
                self.numer -= other .numer.clone();
            } else if ::REDUCE_VIA_LCM {
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= &self.denom;
                let mut b = denom.clone(); b /= &other .denom;

                self.numer *= a;
                self.numer -= &other .numer * b;
                self.denom = denom;
            } else {
                self.numer *= &other .denom;
                self.numer -= other .numer.clone() * &self.denom;
                self.denom *= &other .denom;
            }
        }
    }
    impl Mul<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn mul(self, other : Frac) -> Frac {
            let f = Frac { numer: self.numer * other .numer, denom: self.denom * other .denom };
            f.reduce()
        }
    }
    impl Mul<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn mul(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer * other , denom: self.denom };
            f.reduce()
        }
    }
    impl<'a> MulAssign<&'a Frac> for Frac {
        #[inline]
        fn mul_assign(&mut self, other : &'a Frac) {
            self.numer *= other .numer.clone();
            self.denom *= other .denom.clone();
        }
    }
    impl Div<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn div(self, other : Frac) -> Frac {
            let f = Frac { numer: self.numer * other .denom, denom: self.denom * other .numer };
            f.reduce()
        }
    }
    impl Div<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn div(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer, denom: self.denom * other  };
            f.reduce()
        }
    }

    impl PartialOrd for Frac {
        #[inline]
        fn partial_cmp(&self, other : &Frac) -> Option<cmp::Ordering> {
            let lhs = self.numer.clone() * other .denom.clone();
            let rhs = self.denom.clone() * other .numer.clone();
            lhs.partial_cmp(&rhs)
        }
    }

    impl From<u32> for Frac {
        #[inline]
        fn from(n: u32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<i32> for Frac {
        #[inline]
        fn from(n: i32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<i64> for Frac {
        #[inline]
        fn from(n: i64) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
}

mod frac_type_f64 { pub type Frac = f64; }

mod frac_wrap_f64 {
    use std::cmp;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Frac(f64);
    impl Frac { pub fn bit_length(&self) -> u32 { 64 } }
    impl Frac {
        pub fn sqr(self) -> Self {
            Frac(self.0 * self.0)
        }
    }
    impl Frac {
        pub fn drop_bits(&mut self, _: usize) { /* no op for f64 rep. */ }
    }
    use std::ops::{Add, AddAssign, Sub, SubAssign, Mul, MulAssign, Div};

    impl Add<Frac> for Frac {
        type Output = Frac; fn add(self, other : Frac) -> Frac { Frac(self.0 + other .0) }
    }
    impl<'a> AddAssign<&'a Frac> for Frac {
        fn add_assign(&mut self, other : &'a Frac) { self.0 += other .0; }
    }
    impl Sub<Frac> for Frac {
        type Output = Frac; fn sub(self, other : Frac) -> Frac { Frac(self.0 - other .0) }
    }
    impl<'a> SubAssign<&'a Frac> for Frac {
        fn sub_assign(&mut self, other : &'a Frac) { self.0 -= other .0; }
    }
    impl Mul<Frac> for Frac {
        type Output = Frac; fn mul(self, other : Frac) -> Frac { Frac(self.0 * other .0) }
    }
    impl<'a> MulAssign<&'a Frac> for Frac {
        fn mul_assign(&mut self, other : &'a Frac) { self.0 *= other .0; }
    }
    impl Mul<f64> for Frac {
        type Output = Frac; fn mul(self, other : f64) -> Frac { Frac(self.0 * other ) }
    }
    impl Div<Frac> for Frac {
        type Output = Frac; fn div(self, other : Frac) -> Frac { Frac(self.0 / other .0) }
    }
    impl Div<f64> for Frac {
        type Output = Frac; fn div(self, other : f64) -> Frac { Frac(self.0 / other ) }
    }
    impl Div<i32> for Frac {
        type Output = Frac; fn div(self, other : i32) -> Frac { Frac(self.0 / other  as f64) }
    }

    impl PartialOrd for Frac {
        fn partial_cmp(&self, other : &Frac) -> Option<cmp::Ordering> { self.0.partial_cmp(&other .0) }
    }

    impl From<u32> for Frac { fn from(n: u32) -> Frac { Frac(n as f64) } }
    impl From<i32> for Frac { fn from(n: i32) -> Frac { Frac(n as f64) } }
    impl From<i64> for Frac { fn from(n: i64) -> Frac { Frac(n as f64) } }
    impl From<f64> for Frac { fn from(n: f64) -> Frac { Frac(n) } }
}

mod frac_bigratio {
    use std::cmp;
    use ramp;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Frac { numer: ramp::Int, denom: ramp::Int, }
    impl Frac { pub fn bit_length(&self) -> u32 { self.numer.bit_length() + self.denom.bit_length() } }
    impl Frac { pub fn divide_by_4(&mut self) { self.numer >>= 2 } }
    use std::ops::{Add, AddAssign, Sub, SubAssign, Mul, MulAssign, Div};

    impl Frac {
        fn reduce(self) -> Self {
            let Frac { numer, denom } = self;
            let gcd = numer.gcd(&denom);
            Frac { numer: numer / gcd.clone(), denom: denom / gcd }
            // self
        }
        pub fn sqr(self) -> Self {
            let Frac { numer, denom } = self;
            Frac { numer: numer.dsquare(), denom: denom.dsquare() }
        }
    }

    impl Add<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn add(mut self, other : Frac) -> Frac {
            let f = if self.denom == other .denom {
                self.numer += other .numer;
                self
            } else if ::REDUCE_VIA_LCM {
                // let denom = self.denom.clone() * other .denom.clone();
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= self.denom;
                let mut b = denom.clone(); b /= other .denom;

                self.numer *= a;
                self.numer += other .numer * b;
                self.denom = denom;
                self
            } else {
                Frac { numer: self.numer * other .denom.clone() + other .numer * self.denom.clone(),
                       denom: self.denom * other .denom }
            };
            f.reduce()
        }
    }
    impl Add<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn add(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer + self.denom.clone() * (other  as usize),
                           denom: self.denom };
            f.reduce()
        }
    }

    impl<'a> AddAssign<&'a Frac> for Frac {
        #[inline]
        fn add_assign(&mut self, other : &'a Frac) {
            if self.denom == other .denom {
                self.numer += other .numer.clone();
            } else if ::REDUCE_VIA_LCM {
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= self.denom.clone();
                let mut b = denom.clone(); b /= other .denom.clone();

                self.numer *= a;
                self.numer += other .numer.clone() * b;
                self.denom = denom;
            } else {
                self.numer *= &other .denom;
                self.numer += other .numer.clone() * &self.denom;
                self.denom *= &other .denom;
            }
        }
    }
    impl Sub<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn sub(mut self, other : Frac) -> Frac {
            let f = if self.denom == other .denom {
                self.numer -= other .numer;
                self
            } else if ::REDUCE_VIA_LCM {
                // let denom = self.denom.clone() * other .denom.clone();
                let denom = self.denom.lcm(&other .denom);
                let a = denom.clone() / self.denom;
                let b = denom.clone() / other .denom;
                self.numer *= a;
                self.numer -= other .numer * b;
                self.denom = denom;
                self
            } else {
                Frac { numer: self.numer * other .denom.clone() - other .numer * self.denom.clone(),
                       denom: self.denom * other .denom }
            };
            f.reduce()
        }
    }
    impl Sub<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn sub(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer - self.denom.clone() * (other  as usize),
                           denom: self.denom };
            f.reduce()
        }
    }
    impl<'a> SubAssign<&'a Frac> for Frac {
        #[inline]
        fn sub_assign(&mut self, other : &'a Frac) {
            if self.denom == other .denom {
                self.numer -= other .numer.clone();
            } else if ::REDUCE_VIA_LCM {
                let denom = self.denom.lcm(&other .denom);
                let mut a = denom.clone(); a /= &self.denom;
                let mut b = denom.clone(); b /= &other .denom;

                self.numer *= a;
                self.numer -= &other .numer * b;
                self.denom = denom;
            } else {
                self.numer *= &other .denom;
                self.numer -= other .numer.clone() * &self.denom;
                self.denom *= &other .denom;
            }
        }
    }
    impl Mul<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn mul(self, other : Frac) -> Frac {
            let f = Frac { numer: self.numer * other .numer, denom: self.denom * other .denom };
            f.reduce()
        }
    }
    impl Mul<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn mul(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer * (other  as usize), denom: self.denom };
            f.reduce()
        }
    }
    impl<'a> MulAssign<&'a Frac> for Frac {
        #[inline]
        fn mul_assign(&mut self, other : &'a Frac) {
            self.numer *= other .numer.clone();
            self.denom *= other .denom.clone();
        }
    }
    impl Div<Frac> for Frac {
        type Output = Frac;
        #[inline]
        fn div(self, other : Frac) -> Frac {
            let f = Frac { numer: self.numer * other .denom, denom: self.denom * other .numer };
            f.reduce()
        }
    }
    impl Div<u32> for Frac {
        type Output = Frac;
        #[inline]
        fn div(self, other : u32) -> Frac {
            let f = Frac { numer: self.numer, denom: self.denom * (other  as usize) };
            f.reduce()
        }
    }

    impl PartialOrd for Frac {
        #[inline]
        fn partial_cmp(&self, other : &Frac) -> Option<cmp::Ordering> {
            let lhs = self.numer.clone() * other .denom.clone();
            let rhs = self.denom.clone() * other .numer.clone();
            lhs.partial_cmp(&rhs)
        }
    }

    impl From<u32> for Frac {
        #[inline]
        fn from(n: u32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<i32> for Frac {
        #[inline]
        fn from(n: i32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
}


#[derive(Debug)]
struct Scale {
    /// range for mandelbrot scale [min, max]
    x: [Frac; 2],
    /// range for mandelbrot scale [min, max]
    y: [Frac; 2],
    /// display width
    width: u32,
    /// display height
    height: u32,
}

impl Clone for Scale {
    fn clone(&self) -> Self {
        Scale {
            x: [self.x[0].clone(), self.x[1].clone()],
            y: [self.y[0].clone(), self.y[1].clone()],
            width: self.width,
            height: self.height,
        }
    }
}

fn minmax<O:PartialOrd>(a: O, b: O) -> (O, O) {
    if a < b { (a, b) } else { (b, a) }
}

impl Scale {
    fn zoom_to(&mut self, p1: [f64; 2], p2: [f64; 2]) {
        let old_scale = self.clone();
        let (disp_x1, disp_x2) = minmax(p1[0], p2[0]);
        let (disp_y1, disp_y2) = minmax(p1[1], p2[1]);

        // assert!(disp_x1.fract() <= 1e-10, "x1 frac: {}", disp_x1.fract());
        // assert!(disp_x2.fract() <= 1e-10, "x2 frac: {}", disp_x2.fract());
        // assert!(disp_y1.fract() <= 1e-10, "y1 frac: {}", disp_y1.fract());
        // assert!(disp_y2.fract() <= 1e-10, "y2 frac: {}", disp_y2.fract());
        let disp_x1 = Frac::from(disp_x1.trunc() as i64);
        let disp_x2 = Frac::from(disp_x2.trunc() as i64);
        let disp_y1 = Frac::from(disp_y1.trunc() as i64);
        let disp_y2 = Frac::from(disp_y2.trunc() as i64);
        let disp_width = Frac::from(self.width);
        let disp_height = Frac::from(self.height);
        let old_x1 = self.x[0].clone();
        let old_x2 = self.x[1].clone();
        let old_y1 = self.y[0].clone();
        let old_y2 = self.y[1].clone();
        let old_width =  old_x2.clone() - old_x1.clone();
        let old_height = old_y2.clone() - old_y1.clone();
        let new_x1 = old_x1.clone() + old_width.clone() * disp_x1 / disp_width.clone();
        let new_x2 = old_x1.clone() + old_width.clone() * disp_x2 / disp_width;
        let new_y1 = old_y1.clone() + old_height.clone() * disp_y1 / disp_height.clone();
        let new_y2 = old_y1.clone() + old_height.clone() * disp_y2 / disp_height;

        self.x[0] = new_x1;
        self.x[1] = new_x2;
        self.y[0] = new_y1;
        self.y[1] = new_y2;

        println!("zoom {:?} via {:?} {:?} yields {:?}",
                 old_scale, p1, p2, self);
    }
}

impl Scale {
    fn from_display(&self, x: u32, y: u32) -> (Frac, Frac) {
        debug_assert!(x < self.width);
        debug_assert!(y < self.height);
        let x_delta = self.x[1].clone() - self.x[0].clone();
        let x_offset = x_delta * Frac::from(x) / Frac::from(self.width);
        let y_delta = self.y[1].clone() - self.y[0].clone();
        let y_offset = y_delta * Frac::from(y) / Frac::from(self.height);
        (self.x[0].clone() + x_offset, self.y[0].clone() + y_offset)
    }
}

#[derive(Clone)]
struct Complex(Frac, Frac);
impl Complex {
    pub fn bit_length(&self) -> u32 { self.0.bit_length() + self.1.bit_length() }
    pub fn drop_bits(&mut self, num_bits: usize) {
        self.0.drop_bits((num_bits + 1) / 2);
        self.1.drop_bits((num_bits + 1) / 2);
    }
    #[inline]
    fn mag_less_than_2(&self) -> bool {
        let mut a = self.0.clone().sqr();
        let b = self.1.clone().sqr();
        a += &b;
        a < Frac::from(4)
    }
    #[inline]
    fn mag_less_than(&self, mag: Frac) -> bool {
        let mut a = self.0.clone().sqr();
        let b = self.1.clone().sqr();
        a += &b;
        a < mag.sqr()
    }
    #[inline]
    fn dsquare(&mut self) {
        // (a + b*i)*(a + b*i) = a*a + 2*a*b*i - b*b = a*a - b*b + 2*a*b*i
        let mut y2 = self.1.clone();
        self.1 *= &self.0;
        self.0 = self.0.clone().sqr();
        y2 = y2.sqr();
        self.0 -= &y2;
        self.1 *= &Frac::from(2);
    }
    #[inline]
    fn add(&self, other : &Complex) -> Complex {
        Complex(self.0.clone() + other .0.clone(), self.1.clone() + other .1.clone())
    }
    #[inline]
    fn add_assign(&mut self, other : &Complex) {
        self.0 += &other .0;
        self.1 += &other .1;
    }
}

#[inline]
fn mandelbrot(mut z: Complex, x: u32, y: u32, scale: Scale, max_iters: u32) -> Option<u32> {
    let (x0, y0) = scale.from_display(x, y);
    let c = Complex(x0, y0);
    let mut iteration = 0;
    while iteration < max_iters &&
        z.mag_less_than_2() // z.mag_less_than(Frac::from(2))
    {
    z.dsquare();
        z.add_assign(&c);
        // if z.mag_less_than(Frac::from(1) / Frac::from(2)) { return None; }
        iteration += 1;
    }
    let bits = z.bit_length();
    if bits > 10000 {
        println!("iterations: {} bits: {}", iteration, bits);
    }
    if iteration < max_iters {
        Some(iteration)
    } else {
        None
    }
}

fn iters_to_color(iters: Option<u32>) -> im::Rgba<u8> {
    match iters {
        None => im::Rgba([0, 0, 0, 255]),
        Some(iters) => {
            let lo = (((iters >>  0) & 0b11) << 6) | ((iters >>  4) & 0b1100) << 4;
            let mi = (((iters >>  2) & 0b11) << 6) | ((iters >>  6) & 0b1100) << 4;
            let hi = (((iters >>  4) & 0b11) << 6) | ((iters >>  8) & 0b1100) << 4;
            im::Rgba([lo as u8, mi as u8, (hi % 255) as u8, 255])
        }
    }
}

fn lcm(x: i64, y: i64) -> i64 {
    use num::integer::Integer;
    x * (y / x.gcd(&y))
}

#[test]
fn demo_i64_lcm_problem() {
    let x: i64 = 46656000000000000;
    let y: i64 = 600;
    let l = lcm(x, y);
    println!("x: {} y: {} l: {}", x, y, l);
}
```
