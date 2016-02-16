```rust
extern crate piston_window;
extern crate image as im;
extern crate vecmath;

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

    #[derive(Copy, Clone, Debug)]
    struct DrawSpec {
        scale: Scale,
        width: u32,
        height: u32,
    }

    type IB = im::ImageBuffer<im::Rgba<u8>, Vec<u8>>;

    let mut orig_scale = Scale {
        x: [-2.5, 1.0], y: [-1.0, 1.0], width: width, height: height
        // x: [0.0, 1.0], y: [0.0, 1.0], width: width, height: height
    };
    let mut scale = orig_scale;

    #[derive(Copy, Clone, Debug)]
    enum BgElem { Unknown, InSet, Escapes(u32), }

    let mut background_state: Vec<Cell<BgElem>> =
        vec![Cell::new(BgElem::Unknown); (width * height) as usize];
    const NUM_THREADS: u32 = 8;
    let mut handles_and_ports = vec![];
    let (tx, rx) = channel();

    for i in 0..NUM_THREADS {
        let (tx2, rx2) = channel();
        let tx = tx.clone();
        let handle = thread::spawn(move || {
            let mut scale = scale;
            let work_size = (height + NUM_THREADS - 1) / NUM_THREADS;
            loop {
                let start_y = i * work_size;
                let limit_y = cmp::min(start_y + work_size, height);
                for y in start_y..limit_y {
                    for x in 0..width {
                        let bg_elem = match mandelbrot(x, y, scale) {
                            Some(iters) => BgElem::Escapes(iters),
                            None => BgElem::Unknown,
                        };
                        tx.send((x, y, bg_elem));
                    }
                }
                let spec: DrawSpec = rx2.recv().unwrap();
                scale = spec.scale;
                width = spec.width;
                height = spec.height;
            }
        });
        handles_and_ports.push((handle, tx2));
    }

    let mut process_results = || {
        loop {
            match rx.try_recv() {
                Ok((x, y, bg_elem)) => {
                    let idx = x * width + y;
                    background_state[idx as usize].set(bg_elem);
                }
                Err(e) => break,
            }
        }
    };

    let background = |canvas: &mut IB, scale: Scale| {
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

    process_results();
    background(&mut canvas, scale);
    let mut backup_canvas = canvas.clone();

    texture.update(&mut *window.factory.borrow_mut(), &canvas).unwrap();

    let redo_background = |spec| {
        for c in &background_state { c.set(BgElem::Unknown); }
        for &(_, ref p) in &handles_and_ports { p.send(spec).unwrap(); }
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
        if let Some(idle_args) = e.idle_args() {
            process_results();
            background(&mut canvas, scale);
            backup_canvas = canvas.clone();
            texture.update(&mut *e.factory.borrow_mut(), &canvas).unwrap();
        } else if let Some(s) = e.text_args() {
            match &s[..] {
                "r" => {
                    scale = orig_scale;
                    redo_background(DrawSpec {
                        scale: scale,
                        width: width,
                        height: height,
                    });
                }
                "-" => {
                    let w_2 = (scale.x[1] - scale.x[0]) / 2.0;
                    let h_2 = (scale.y[1] - scale.y[0]) / 2.0;
                    scale = Scale {
                        x: [scale.x[0] - w_2, scale.x[1] + w_2],
                        y: [scale.y[0] - h_2, scale.y[1] + h_2],
                        ..scale
                    };
                    redo_background(DrawSpec {
                        scale: scale,
                        width: width,
                        height: height,
                    })
                }
                "+" => {
                    let w_4 = (scale.x[1] - scale.x[0]) / 4.0;
                    let h_4 = (scale.y[1] - scale.y[0]) / 4.0;
                    scale = Scale {
                        x: [scale.x[0] + w_4, scale.x[1] - w_4],
                        y: [scale.y[0] + h_4, scale.y[1] - h_4],
                        ..scale
                    };
                    redo_background(DrawSpec {
                        scale: scale,
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
            redo_background(DrawSpec { scale: scale, width: width, height: height, });
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

// FIXME: I wanted to use fixed-point fractions, not floating.
type Frac = f64;

#[derive(Copy, Clone, Debug)]
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

fn minmax<O:PartialOrd>(a: O, b: O) -> (O, O) {
    if a < b { (a, b) } else { (b, a) }
}

impl Scale {
    fn zoom_to(&mut self, p1: [f64; 2], p2: [f64; 2]) {
        let old_scale = *self;
        let (disp_x1, disp_x2) = minmax(p1[0], p2[0]);
        let (disp_y1, disp_y2) = minmax(p1[1], p2[1]);

        let disp_width = self.width as f64;
        let disp_height = self.height as f64;
        let old_x1 = self.x[0]; let old_x2 = self.x[1];
        let old_y1 = self.y[0]; let old_y2 = self.y[1];
        let old_width = old_x2 - old_x1;
        let old_height = old_y2 - old_y1;
        let new_x1 = old_x1 + disp_x1 / disp_width * old_width;
        let new_x2 = old_x1 + disp_x2 / disp_width * old_width;
        let new_y1 = old_y1 + disp_y1 / disp_height * old_height;
        let new_y2 = old_y1 + disp_y2 / disp_height * old_height;

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
        let x_delta = self.x[1] - self.x[0];
        let x_offset = x_delta * (x as Frac) / (self.width as Frac);
        let y_delta = self.y[1] - self.y[0];
        let y_offset = y_delta * (y as Frac) / (self.height as Frac);
        (self.x[0] + x_offset, self.y[0] + y_offset)
    }
}

struct Complex(Frac, Frac);
impl Complex {
    #[inline]
    fn mag_less_than(&self, mag: Frac) -> bool {
        self.0 * self.0 + self.1 * self.1 < mag * mag
    }
    #[inline]
    fn sqr(&self) -> Complex {
        // (a + b*i)*(a + b*i) = a*a + 2*a*b*i - b*b = a*a - b*b + 2*a*b*i
        let Complex(x,y) = *self;
        Complex(x*x - y*y, 2.0 * x * y)
    }
    #[inline]
    fn add(&self, other: &Complex) -> Complex {
        Complex(self.0 + other.0, self.1 + other.1)
    }
}

fn mandelbrot(x: u32, y: u32, scale: Scale) -> Option<u32> {
    let (x0, y0) = scale.from_display(x, y);
    let c = Complex(x0, y0);
    let mut z = Complex(0.0, 0.0);
    let mut iteration = 0;
    const MAX_ITERATION: u32 = 0xF_FF;
    while iteration < MAX_ITERATION && z.mag_less_than(2.0) {
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
