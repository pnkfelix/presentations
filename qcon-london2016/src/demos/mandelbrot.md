```rust
#![feature(float_extras, augmented_assignments, op_assign_traits)]
#![allow(dead_code)]

extern crate piston_window;
extern crate image as im;
extern crate vecmath;
extern crate ramp;
extern crate num;
#[cfg(feature = "gmp")]
extern crate gmp;
extern crate stopwatch;

const REDUCE_VIA_LCM: bool = false;
const TOLERANCE: f64 = 0.0000000001;

use im::GenericImage;
use piston_window::*;
// use im::Pixel;
use stopwatch::Stopwatch;

use std::cell::Cell;
use std::cmp;
use std::thread;
use std::sync::mpsc::channel;

// macro_rules! db { ($($e:expr),*) => { println!($($e),*) } }
macro_rules! db { ($($e:expr),*) => { } }

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

    let zoom_scale = Scale {
        x: [Frac::from(-0.7387733983830036),  Frac::from(-0.7387733983830016)],
        y: [Frac::from(-0.13407787050277506), Frac::from(-0.13407787050277412)],
        width: width,
        height: height
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
        zoom_scale.clone()
        // zoomed_scale
    };

    #[derive(Copy, Clone, Debug)]
    enum BgElem { Unknown, InSet, Escapes(u32), }

    let background_state: Vec<Cell<BgElem>> =
        vec![Cell::new(BgElem::Unknown); (width * height) as usize];
    const NUM_THREADS: u32 = 8;
    let mut handles_and_ports = vec![];
    let (tx, rx) = channel();

    for i in 0..NUM_THREADS {
        let (tx2, rx2) = channel();
        let tx = tx.clone();
        let mut scale = scale.clone();
        let handle = thread::spawn(move || {
            let work_size = (height + NUM_THREADS - 1) / NUM_THREADS;

            const MAX_ITERATION: u32 = 0x1_00_00_00;
            const ITER_INCR: u32 = 0x1_00;
            const ITER_FACTOR: u32 = 4;
            const ITER_START: u32 = 0x10_00;
            // const ITER_INCR: u32 = 0x1;

            // let mut max_iters = 65536;
            let mut reset_min = ITER_START;
            let mut max_iters = ITER_START;

            'draw: loop {

                while max_iters < MAX_ITERATION {
                    db!("thread: {} max_iters: 0x{:<8x} = {}", i, max_iters, max_iters);
                    let start_y = i * work_size;
                    let limit_y = cmp::min(start_y + work_size, height);
                    let mut saw_content = false;

                    let _sw = Stopwatch::start_new();
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

                                // FIXME: move to top of 'draw: loop
                                db!("switching from scale: {:?} to scale: {:?}", scale, spec.scale);

                                let lo_x = spec.scale.x[0] >= scale.x[0];
                                let hi_x = spec.scale.x[1] <= scale.x[1];
                                let lo_y = spec.scale.y[0] >= scale.y[0];
                                let hi_y = spec.scale.y[1] <= scale.y[1];
                                let in_bounds = lo_x && hi_x && lo_y && hi_y;
                                if in_bounds {
                                    db!("in bounds");
                                } else {
                                    db!("!in bounds; resetting reset_min from {} to {}", reset_min, ITER_START);
                                    reset_min = ITER_START;
                                }

                                max_iters = reset_min;
                                scale = spec.scale;
                                width = spec.width;
                                height = spec.height;
                                continue 'draw;
                            }
                        }
                    }

                    db!("thread: {} iterations: {} time: {:?}", i, max_iters, _sw.elapsed());
                    if !saw_content {
                        max_iters *= ITER_FACTOR;
                        reset_min = max_iters;
                    } else {
                        max_iters += ITER_INCR;
                    }
                }

                // if we finish, then just wait for a new command to come in.
                let spec: DrawSpec = rx2.recv().unwrap();

                // FIXME: move to top of 'draw: loop
                db!("switching from scale: {:?} to scale: {:?}", scale, spec.scale);

                let in_bounds =
                    spec.scale.x[0] >= scale.x[0] && spec.scale.x[1] <= scale.x[0] &&
                    spec.scale.y[0] >= scale.y[0] && spec.scale.y[1] <= scale.y[0];

                if in_bounds {
                    db!("in bounds");
                } else {
                    db!("!in bounds; resetting reset_min from {} to {}", reset_min, ITER_START);
                    reset_min = ITER_START;
                }

                max_iters = reset_min;
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
                        zoom_scale.clone()
                    } else {
                        orig_scale.clone()
                    };
                    let spec = DrawSpec {
                        scale: scale.clone(),
                        width: width,
                        height: height,
                    };
                    db!("resetting to spec: {:?} under mode: {:?}", spec, mode);
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
            db!("press yields mode: {:?}", mode);
        } else if let Some(_button) = e.release_args() {
            if let (Mode::DrawingRect(last), Some(last_pos)) = (mode, last_pos) {
                if last_pos[0] < 0.0 || last_pos[1] < 0.0 {
                    db!("ignore release due to negative values in last_pos");
                    mode = Mode::Waiting;
                } else {
                    mode = Mode::ZoomTo(last, last_pos);
                }
            } else {
                mode = Mode::Waiting;
            }
            db!("release yields mode: {:?}", mode);
        } else if let (Mode::NextDrawRect, Some(pos)) = (mode, args) {
            mode = Mode::DrawingRect(pos);
        } else if let Some(_resize) = e.resize_args() {
            db!("window resized: {:?}", _resize);
        }

        if let Mode::ZoomTo(p1, p2) = mode {
            db!("zooming to mode: {:?}", mode);
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
// use frac_wrap_f64::Frac;
// use frac_bigratio::Frac;
// use frac_dynamic::Frac;
// use frac_mpq::Frac;
// use frac_florat::Frac;
// use frac_fixrat::Frac;
// use frac_limit_bigratio::Frac;
use frac_bampf::Frac;

mod frac_bampf {
    use std::cmp;
    use std::ops;

    /// The Frac here represents the number `x_man * num`.
    /// Thus the `x_man` is acting as extra bits of mantissaa.
    ///
    /// The components of the product are not guaranteed to be
    /// normalized; thus two Frac's with distinct values for `x_man`
    /// and `num` may actually represent the same number (at least up
    /// to the error tolerance levels permitted by the floating-point
    /// component.)
    ///
    /// However, we *do* guarantee in this representation that
    /// `x_man` is non-zero.
    #[derive(Copy, Clone, Debug)]
    pub struct Frac { x_man: u64, num: f64 }
    impl Frac {
        fn new(x: u64, n: f64) -> Frac {
            assert!(x > 0);
            Frac { x_man: x, num: n }
        }
    }
    impl Frac { pub fn bit_length(&self) -> u32 { 128 } }
    impl Frac { pub fn drop_bits(&mut self, _: usize) { /* no op */ } }
    impl Frac { pub fn sqr(self) -> Self { self * self } }
    use std::ops::{Add, Sub, Mul, Div};

    impl Frac { pub fn to_f64(&self) -> f64 { self.x_man as f64 * self.num } }

    impl cmp::PartialEq for Frac {
        fn eq(&self, other: &Frac) -> bool {
            self.to_f64().eq(&other.to_f64())
        }
        #[cfg(v1)]
        fn eq(&self, other: &Frac) -> bool {
            use std::u64;
            //      N * F == N' * F'
            // <==> N * F/F' == N'
            let lhs = (self.x_man as f64 * (self.num / other.num));
            if lhs > (u64::MAX as f64) { return false; }
            if lhs < (u64::MIN as f64) { return false; }
            (lhs as u64).eq(&other.x_man)
        }
    }

    impl cmp::PartialOrd for Frac {
        fn partial_cmp(&self, other: &Frac) -> Option<cmp::Ordering> {
            self.to_f64().partial_cmp(&other.to_f64())
        }
        #[cfg(v1)]
        fn partial_cmp(&self, other: &Frac) -> Option<cmp::Ordering> {
            use std::u64;
            //      N * F < N' * F'
            // <==> N * F/F' < N'
            let lhs = (self.x_man as f64 * (self.num / other.num));
            if lhs > (u64::MAX as f64) { return Some(cmp::Ordering::Greater); }
            if lhs < (u64::MIN as f64) { return Some(cmp::Ordering::Less); }
            (lhs as u64).partial_cmp(&other.x_man)
        }
    }

    impl Add for Frac {
        type Output = Frac;
        fn add(self, other: Frac) -> Frac {
            // N * F + N' * F'
            //
            // assume w.l.o.g that abs(F) > abs(F')
            //
            // = N * F + N * (N'/N * F')
            // = N * (F + N'/N * F')
            // = N * (F + (floor(N'/N) + (N' % N) * 1/N) * F')
            // = N * (F + floor(N'/N) * F' + (N' % N) * 1/N * F')

            // Goal: preserve the x_man with more significant digits
            if other.x_man.leading_zeros() < self.x_man.leading_zeros() {
                return other + self;
            }

            let n = self.x_man;
            let s_f = self.num;
            let o_f = other.num;
            let div = other.x_man / n;
            let rem = other.x_man % n;
            let ret = Frac { x_man: n,
                             num: s_f + (div as f64 * o_f) + rem as f64 * (o_f / n as f64) };
            // let ret_alt = self.to_f64() + other.to_f64();
            //
            // if (ret.to_f64() - ret_alt).abs() > ::TOLERANCE {
            //     db!("ADD {:?} TO {:?} => {:?} (VERSUS {})",
            //              self, other, ret, ret_alt);
            // } else {
            //     db!("add {:?} to {:?} => {:?} (versus {})",
            //              self, other, ret, ret_alt);
            // }
            ret
        }
    }

    impl Sub for Frac {
        type Output = Frac;
        fn sub(self, other: Frac) -> Frac {
            self + Frac { x_man: other.x_man, num: -other.num }
        }
    }

    impl Frac {
        fn half_precision(self) -> Frac {
            // (N * 2^32 + N2) * F
            // = N * 2^32 * F + N2 * F
            // = N * 2^32 * F * (1 + N2/(N * 2^32))
            // = N * (2^32 * F + 2^32 * F * N2/(N * 2^32))
            // = N * (2^32 * F + F * N2/N)

            let hi = self.x_man >> 32;
            let lo = self.x_man & !(!0 << 32);
            let two_to_32 = (1u64 << 32) as f64;
            let f = self.num;
            assert!(hi > 0);
            let new = Frac { x_man: hi,
                             num: two_to_32 * f + (f * lo as f64) / hi as f64 };
            // db!("half_precision of {:?} is {:?}",
            //          self, new);
            return new;
        }
    }

    impl Mul for Frac {
        type Output = Frac;
        fn mul(self, other: Frac) -> Frac {
            let ret;

            if other.num == 0.0 || self.num == 0.0 {
                ret = Frac { x_man: 1, num: 0.0 };
            }
            // N * F * N' * F'
            // = N * N' * F * F'
            else if let (prod, false) = self.x_man.overflowing_mul(other.x_man) {
                ret = Frac { x_man: prod, num: self.num * other.num }
            } else {

                // otherwise x_man overflows; move half the bits of
                // each into the corresponding float to ensure that
                // next multiplication will succeed.

                match (self.x_man >> 32, other.x_man >> 32) {
                    (s_high, o_high) if s_high != 0 && o_high != 0 => {
                        ret = self.half_precision() * other.half_precision();
                    }
                    (s_high, 0) if s_high != 0 => {
                        ret =self.half_precision() * other;
                    }
                    (0, o_high) if o_high != 0 => {
                        ret = self * other.half_precision();
                    }
                    (_, _) => {
                        panic!("how could we have overflowed in this case?");
                    }
                }
            }

            // let ret_alt = self.to_f64() * other.to_f64();
            //
            // if (ret.to_f64() - ret_alt).abs() > ::TOLERANCE {
            //     db!("MUL {:?} BY {:?} => {:?} (VERSUS {})",
            //              self, other, ret, ret_alt);
            // } else {
            //     db!("mul {:?} by {:?} => {:?} (versus {})",
            //              self, other, ret, ret_alt);
            // }
            return ret;
        }
    }

    impl Div for Frac {
        type Output = Frac;
        fn div(self, other: Frac) -> Frac {
            let ret;

            // N * F / N' * F'
            // = N / N' * F / F'
            //
            // but note that integer division would throw away bits.
            // we can do better via div rem:
            //
            // = (floor(N / N') + N % N' * 1/N') * F / F'
            // = (floor(N / N') * F / F' + (N % N' * 1/N') * F / F')
            // = floor(N / N') * (F / F' + (N % N' * 1/N') * F / F' / floor(N/N'))

            let div = self.x_man / other.x_man;
            let rem = self.x_man % other.x_man;
            let f_over_f = self.num / other.num;

            if other.num != 0.0 && self.num == 0.0 {
                ret = Frac { x_man: 1, num: 0.0 };
            } else if div == 0 {
                // ensure x_man does not go to zero
                ret = Frac { x_man: rem, num: f_over_f / other.x_man as f64 };
            } else {
                ret = Frac {
                    x_man: div,
                    num: (f_over_f + ((rem as f64 * (f_over_f / other.x_man as f64)) / div as f64))
                };
            }

            // let ret_alt = self.to_f64() / self.to_f64();
            // if (ret.to_f64() - ret_alt).abs() > ::TOLERANCE {
            //     db!("DIV {:?} BY {:?} => {:?} (VERSUS {})",
            //              self, other, ret, ret_alt);
            // } else {
            //     db!("div {:?} by {:?} => {:?} (versus {})",
            //              self, other, ret, ret_alt);
            // }
            return ret;
        }
    }

    impl Div<i32> for Frac {
        type Output = Frac;
        fn div(self, other: i32) -> Frac { self / Frac::from(other) }
    }


    impl From<u32> for Frac {
        fn from(n: u32) -> Frac {
            if n == 0 {
                Frac { x_man: 1, num: n as f64 }
            } else {
                Frac { x_man: n as u64, num: 1.0 }
            }
        }
    }
    impl From<i32> for Frac {
        fn from(n: i32) -> Frac {
            if n == 0 {
                Frac { x_man: 1, num: n as f64 }
            } else if n < 0 {
                Frac { x_man: -n as u64, num: -1.0 }
            } else {
                Frac { x_man: n as u64, num: 1.0 }
            }
        }
    }
    impl From<f64> for Frac {
        fn from(f: f64) -> Frac {
            let (mant, exp, sign) = f.integer_decode();
            let num = 2.0f64;
            let frac = num.powi(exp as i32);
            Frac { x_man: mant, num: if sign < 0 { -frac } else { frac } }
        }
    }
    impl From<u64> for Frac {
        fn from(n: u64) -> Frac {
            if n == 0 { Frac { x_man: 1, num: n as f64 } } else { Frac { x_man: n, num: 1.0 } }
        }
    }
    impl From<i64> for Frac {
        fn from(n: i64) -> Frac {
            if n == 0 {
                Frac { x_man: 1, num: n as f64 }
            } else if n < 0 {
                Frac { x_man: -n as u64, num: -1.0 }
            } else {
                Frac { x_man: n as u64, num: 1.0 }
            }
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
}

mod frac_limit_bigratio {
    use std::cmp;
    use ramp;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Frac { numer: ramp::Int, denom: ramp::Int, }
    impl Frac { pub fn bit_length(&self) -> u32 { self.numer.bit_length() + self.denom.bit_length() } }
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
                db!("reduce self: {:?} bits: {}", self, bits);
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
    impl From<f64> for Frac {
        #[inline]
        fn from(f: f64) -> Frac {
            let (mant, mut exp, sign) = f.integer_decode();
            let mut num = Frac::from(1 as i32);
            while exp > 0 {
                if exp % 2 == 0 {
                    num = num.clone() * num;
                    exp /= 2;
                } else {
                    num = num * Frac::from(2 as i32);
                    exp -= 1;
                }
            }
            if sign < 0 {
                num = Frac::from(0) - num;
            }
            num = num * Frac::from(mant as i64);
            num
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
                db!("counts: {:?}", &counts[1..]);
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

#[cfg(feature = "gmp")]
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
    impl From<f64> for Frac {
        fn from(f: f64) -> Frac {
            let (mant, mut exp, sign) = f.integer_decode();
            let mut num = Mpq::from(1 as i64);
            while exp > 0 {
                if exp % 2 == 0 {
                    num = num.clone() * num;
                    exp /= 2;
                } else {
                    num = num * Mpq::from(2 as i64);
                    exp -= 1;
                }
            }
            if sign < 0 {
                num = -num;
            }
            num = num * Mpq::from(mant as i64);
            Frac(num)
        }
    }

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
    impl From<i64> for Frac {
        #[inline]
        fn from(n: i64) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<f64> for Frac {
        #[inline]
        fn from(f: f64) -> Frac {
            let (mant, mut exp, sign) = f.integer_decode();
            let mut num = Frac::from(1 as i32);
            while exp > 0 {
                if exp % 2 == 0 {
                    num = num.clone() * num;
                    exp /= 2;
                } else {
                    num = num * Frac::from(2 as i32);
                    exp -= 1;
                }
            }
            if sign < 0 {
                num = Frac::from(0) - num;
            }
            num = num * Frac::from(mant as i64);
            num
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
        let _old_scale = self.clone();
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

        db!("zoom {:?} via {:?} {:?} yields {:?}",
            _old_scale, p1, p2, self);
    }
}

impl Scale {
    fn from_display(&self, x: u32, y: u32) -> (Frac, Frac) {
        debug_assert!(x < self.width);
        debug_assert!(y < self.height);
        let x_delta = self.x[1].clone() - self.x[0].clone();
        let x_offset = x_delta * (Frac::from(x) / Frac::from(self.width));
        let y_delta = self.y[1].clone() - self.y[0].clone();
        let y_offset = y_delta * (Frac::from(y) / Frac::from(self.height));
        (self.x[0].clone() + x_offset, self.y[0].clone() + y_offset)
    }
}

#[derive(Clone, Debug)]
struct Complex(Frac, Frac);
impl Complex {
    pub fn bit_length(&self) -> u32 { self.0.bit_length() + self.1.bit_length() }
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
        iteration += 1;
    }
    // let bits = z.bit_length();
    // if bits > 10000 {
    //     db!("iterations: {} bits: {}", iteration, bits);
    // }

    // db!("mandelbrot: x: {} y: {} => (x0, y0): {:?} iters: {}", x, y, c, iteration);

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
