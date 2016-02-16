```rust
#![feature(float_extras)]

extern crate piston_window;
extern crate image as im;
extern crate vecmath;
extern crate ramp;

use im::GenericImage;
use piston_window::*;
// use im::Pixel;

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

    let zoomed_scale = Scale {
        // x: [Frac::from(-2.5), Frac::from(1)],
        x: [Frac::from(-0.7387733983830036),  Frac::from(-0.7387733983830016)],
        y: [Frac::from(-0.13407787050277506), Frac::from(-0.13407787050277412)],
        width: width,
        height: height
        // x: [0.0, 1.0], y: [0.0, 1.0], width: width, height: height
    };
    let orig_scale = Scale {
        x: [Frac::from(-5) / Frac::from(2),  Frac::from(1)],
        y: [Frac::from(-1), Frac::from(1)],
        width: width,
        height: height
        // x: [0.0, 1.0], y: [0.0, 1.0], width: width, height: height
    };
    let mut scale = orig_scale.clone();

    #[derive(Copy, Clone, Debug)]
    enum BgElem { Unknown, _InSet, Escapes(u32), }

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
            'draw: loop {
                let start_y = i * work_size;
                let limit_y = cmp::min(start_y + work_size, height);
                for y in start_y..limit_y {
                    for x in 0..width {
                        let bg_elem = match mandelbrot(x, y, scale.clone()) {
                            Some(iters) => BgElem::Escapes(iters),
                            None => BgElem::Unknown,
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
                    BgElem::_InSet => None,
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
                "r" => {
                    redo_background(DrawSpec {
                        scale: scale.clone(),
                        width: width,
                        height: height,
                    });
                }
                "-" => {
                    let x0 = scale.x[0].clone();
                    let x1 = scale.x[1].clone();
                    let y0 = scale.y[0].clone();
                    let y1 = scale.y[1].clone();
                    let w_2 = (x1.clone() - x0.clone()) / 2.0;
                    let h_2 = (y1.clone() - y0.clone()) / 2.0;
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
                    let w_4 = (x1.clone() - x0.clone()) / 4.0;
                    let h_4 = (y1.clone() - y0.clone()) / 4.0;
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

use frac_type_f64::Frac;

mod frac_type_f64 { pub type Frac = f64; }

mod frac_wrap_f64 {
    use std::cmp;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Frac(f64);

    use std::ops::{Add, Sub, Mul, Div};

    impl Add<Frac> for Frac {
        type Output = Frac; fn add(self, other: Frac) -> Frac { Frac(self.0 + other.0) }
    }
    impl Sub<Frac> for Frac {
        type Output = Frac; fn sub(self, other: Frac) -> Frac { Frac(self.0 - other.0) }
    }
    impl Mul<Frac> for Frac {
        type Output = Frac; fn mul(self, other: Frac) -> Frac { Frac(self.0 * other.0) }
    }
    impl Mul<f64> for Frac {
        type Output = Frac; fn mul(self, other: f64) -> Frac { Frac(self.0 * other) }
    }
    impl Div<Frac> for Frac {
        type Output = Frac; fn div(self, other: Frac) -> Frac { Frac(self.0 / other.0) }
    }
    impl Div<f64> for Frac {
        type Output = Frac; fn div(self, other: f64) -> Frac { Frac(self.0 / other) }
    }

    impl PartialOrd for Frac {
        fn partial_cmp(&self, other: &Frac) -> Option<cmp::Ordering> { self.0.partial_cmp(&other.0) }
    }

    impl From<u32> for Frac { fn from(n: u32) -> Frac { Frac(n as f64) } }
    impl From<i32> for Frac { fn from(n: i32) -> Frac { Frac(n as f64) } }

    impl From<f64> for Frac {
        fn from(f: f64) -> Frac {
            use ramp::Int;
            let (mantissa, exponent, sign): (u64, i16, i8) = f.integer_decode();
            let mantissa = mantissa as i64;
            let mantissa = if sign < 0 { -mantissa } else { mantissa };
            let mantissa = Int::from(mantissa);
            let (_numer, _denom) = if exponent < 0 {
                let e = -exponent as usize;
                (mantissa, Int::from(2).pow(e))
            } else {
                let e = exponent as usize;
                (mantissa * Int::from(2).pow(e), Int::from(1))
            };
            Frac(f)
        }
    }
}

mod frac_bigratio {
    use std::cmp;
    use ramp;

    #[derive(Clone, Debug, PartialEq)]
    pub struct Frac { numer: ramp::Int, denom: ramp::Int, }

    use std::ops::{Add, Sub, Mul, Div};

    impl Add<Frac> for Frac {
        type Output = Frac;
        fn add(self, other: Frac) -> Frac {
            if self.denom == other.denom {
                Frac { numer: self.numer + other.numer, denom: self.denom }
            } else {
                Frac { numer: self.numer * other.denom.clone() + other.numer * self.denom.clone(),
                       denom: self.denom * other.denom }
            }
        }
    }
    impl Add<u32> for Frac {
        type Output = Frac;
        fn add(self, other: u32) -> Frac {
            Frac { numer: self.numer + self.denom.clone() * (other as usize),
                   denom: self.denom }
        }
    }
    impl Sub<Frac> for Frac {
        type Output = Frac;
        fn sub(self, other: Frac) -> Frac {
            if self.denom == other.denom {
                Frac { numer: self.numer - other.numer, denom: self.denom }
            } else {
                Frac { numer: self.numer * other.denom.clone() - other.numer * self.denom.clone(),
                       denom: self.denom * other.denom }
            }
        }
    }
    impl Sub<u32> for Frac {
        type Output = Frac;
        fn sub(self, other: u32) -> Frac {
            Frac { numer: self.numer - self.denom.clone() * (other as usize),
                   denom: self.denom }
        }
    }
    impl Mul<Frac> for Frac {
        type Output = Frac;
        fn mul(self, other: Frac) -> Frac {
            Frac { numer: self.numer * other.numer, denom: self.denom * other.denom }
        }
    }
    impl Mul<u32> for Frac {
        type Output = Frac;
        fn mul(self, other: u32) -> Frac {
            Frac { numer: self.numer * (other as usize), denom: self.denom }
        }
    }
    impl Div<Frac> for Frac {
        type Output = Frac;
        fn div(self, other: Frac) -> Frac {
            Frac { numer: self.numer * other.denom, denom: self.denom * other.numer }
        }
    }
    impl Div<u32> for Frac {
        type Output = Frac;
        fn div(self, other: u32) -> Frac {
            Frac { numer: self.numer, denom: self.denom * (other as usize) }
        }
    }

    impl PartialOrd for Frac {
        fn partial_cmp(&self, other: &Frac) -> Option<cmp::Ordering> {
            let lhs = self.numer.clone() * other.denom.clone();
            let rhs = self.denom.clone() * other.numer.clone();
            lhs.partial_cmp(&rhs)
        }
    }

    impl From<u32> for Frac {
        fn from(n: u32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<i32> for Frac {
        fn from(n: i32) -> Frac {
            Frac { numer: From::from(n), denom: From::from(1) }
        }
    }
    impl From<f64> for Frac {
        fn from(f: f64) -> Frac {
            use ramp::Int;
            let (mantissa, exponent, sign): (u64, i16, i8) = f.integer_decode();
            let mantissa = mantissa as i64;
            let mantissa = if sign < 0 { -mantissa } else { mantissa };
            let mantissa = Int::from(mantissa);
            let (numer, denom) = if exponent < 0 {
                let e = -exponent as usize;
                (mantissa, Int::from(2).pow(e))
            } else {
                let e = exponent as usize;
                (mantissa * Int::from(2).pow(e), Int::from(1))
            };
            Frac { numer: numer, denom: denom }
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

        let disp_width = self.width as f64;
        let disp_height = self.height as f64;
        let old_x1 = self.x[0].clone();
        let old_x2 = self.x[1].clone();
        let old_y1 = self.y[0].clone();
        let old_y2 = self.y[1].clone();
        let old_width =  old_x2.clone() - old_x1.clone();
        let old_height = old_y2.clone() - old_y1.clone();
        let new_x1 = old_x1.clone() + old_width.clone() * disp_x1 / disp_width;
        let new_x2 = old_x1.clone() + old_width.clone() * disp_x2 / disp_width;
        let new_y1 = old_y1.clone() + old_height.clone() * disp_y1 / disp_height;
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
    #[inline]
    fn mag_less_than(&self, mag: Frac) -> bool {
        self.0.clone() * self.0.clone() + self.1.clone() * self.1.clone() < mag.clone() * mag
    }
    #[inline]
    fn sqr(&self) -> Complex {
        // (a + b*i)*(a + b*i) = a*a + 2*a*b*i - b*b = a*a - b*b + 2*a*b*i
        let Complex(x,y) = self.clone();
        Complex(x.clone()*x.clone() - y.clone()*y.clone(), Frac::from(2) * x * y)
    }
    #[inline]
    fn add(&self, other: &Complex) -> Complex {
        Complex(self.0.clone() + other.0.clone(), self.1.clone() + other.1.clone())
    }
}

fn mandelbrot(x: u32, y: u32, scale: Scale) -> Option<u32> {
    let (x0, y0) = scale.from_display(x, y);
    let c = Complex(x0, y0);
    let mut z = Complex(Frac::from(0), Frac::from(0));
    let mut iteration = 0;
    const MAX_ITERATION: u32 = 0xF_FF;
    // const MAX_ITERATION: u32 = 0xFF_FF_FF;
    while iteration < MAX_ITERATION && z.mag_less_than(Frac::from(2)) {
        z = z.sqr().add(&c);
        iteration += 1;
    }
    if iteration < MAX_ITERATION {
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
```
