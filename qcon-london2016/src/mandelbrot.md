```rust
extern crate piston_window;
extern crate image as im;
extern crate vecmath;

use im::GenericImage;
use piston_window::*;
// use im::Pixel;

use std::cmp;

fn main() {
    let opengl = OpenGL::V3_2;
    let (width, height) = (300, 300);
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

    type IB = im::ImageBuffer<im::Rgba<u8>, Vec<u8>>;
    let background = |canvas: &mut IB, scale: Scale| {
        for (x, y, p) in canvas.enumerate_pixels_mut() {
            // p.blend(&im::Rgba([(x % 255) as u8, (y % 255) as u8, 0, 128]));
            // *p = im::Rgba([(x % 255) as u8, (y % 255) as u8, 0, 128]);
            *p = iters_to_color(mandelbrot(x, y, scale));
        }
    };

    let orig_scale = Scale {
        x: [-2.5, 1.0], y: [-1.0, 1.0], width: width, height: height
        // x: [0.0, 1.0], y: [0.0, 1.0], width: width, height: height
    };
    let mut scale = orig_scale;
    background(&mut canvas, scale);
    let mut backup_canvas = canvas.clone();

    texture.update(&mut *window.factory.borrow_mut(), &canvas).unwrap();

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
        if let Some(s) = e.text_args() {
            if s == "r" {
                scale = orig_scale;
                background(&mut canvas, scale);
                backup_canvas = canvas.clone();
                texture.update(&mut *e.factory.borrow_mut(), &canvas).unwrap();
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
        }

        if let Mode::ZoomTo(p1, p2) = mode {
            scale.zoom_to(p1, p2);
            background(&mut canvas, scale);
            backup_canvas = canvas.clone();
            texture.update(&mut *e.factory.borrow_mut(), &canvas).unwrap();
            mode = Mode::Waiting;
        }

        let in_range = |curr: [f64; 2]| {
            let x = curr[0]; let y = curr[1];
            0f64 <= x && x < (width as f64) && 0f64 <= y && y < (height as f64)
        };

        if let (Mode::DrawingRect(start), Some(curr)) = (mode, args) {
            if !in_range(curr) { continue; }
            canvas.copy_from(&backup_canvas, 0, 0);

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
            texture.update(&mut*e.factory.borrow_mut(), &canvas).unwrap();
        }
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

fn mandelbrot(x: u32, y: u32, scale: Scale) -> Option<u32> {
    let (x0, y0) = scale.from_display(x, y);
    let (mut x, mut y) = (0.0, 0.0);
    let mut iteration = 0;
    const MAX_ITERATION: u32 = 0xF_FF;
    while x*x + y*y < 2.0*2.0 && iteration < MAX_ITERATION {
        let x_temp = x*x - y*y + x0;
        y = 2.0*x*y + y0;
        x = x_temp;
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
