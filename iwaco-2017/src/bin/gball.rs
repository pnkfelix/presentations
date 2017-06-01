extern crate cairo;
extern crate gtk;
extern crate rayon;

use gtk::prelude::*;
use gtk::{Button, Window, WindowType};
use gtk::DrawingArea;

use cairo::enums::{FontSlant, FontWeight};
use cairo::Context;

use std::f64::consts::PI;
use std::process;

fn main() {
    if gtk::init().is_err() {
        println!("Failed to initialize GTK.");
        process::exit(1);
    }

    drawable(500, 500, |_, cr| {
        cr.set_dash(&[3., 2., 1.], 1.);

        cr.scale(500f64, 500f64);

        cr.set_source_rgb(250.0/255.0,
                          254.0/255.0,
                          55.0/255.0);
        cr.paint();

        cr.set_line_width(0.05);

        // border
        cr.set_source_rgb(0.3, 0.3, 0.3);
        cr.rectangle(0.0, 0.0, 1.0, 1.0);
        cr.stroke();

        cr.set_line_width(0.03);

        // draw circle
        cr.arc(0.5, 0.5, 0.4, 0.0, PI * 2.);
        cr.stroke();

        // mouth
        let mouth_top = 0.68;
        let mouth_width = 0.38;

        let mouth_dx = 0.10;
        let mouth_dy = 0.10;

        cr.move_to( 0.50 - mouth_width/2.0, mouth_top);
        cr.curve_to(0.50 - mouth_dx, mouth_top + mouth_dy,
                    0.50 + mouth_dx, mouth_top + mouth_dy,
                    0.50 + mouth_width/2.0, mouth_top);

        println!("Extents: {:?}", cr.fill_extents());

        cr.stroke();

        let eye_y = 0.38;
        let eye_dx = 0.15;
        cr.arc(0.5 - eye_dx, eye_y, 0.05, 0.0, PI * 2.);
        cr.fill();

        cr.arc(0.5 + eye_dx, eye_y, 0.05, 0.0, PI * 2.);
        cr.fill();

        Inhibit(false)
    });

    drawable(500, 500, |_, cr| {
        cr.scale(500_f64, 500_f64);

        cr.select_font_face("Sans", FontSlant::Normal, FontWeight::Normal);
        cr.set_font_size(0.35);

        cr.move_to(0.04, 0.53);
        cr.show_text("Hello");

        cr.move_to(0.27, 0.65);
        cr.text_path("void");
        cr.set_source_rgb(0.5, 0.5, 1.0);
        cr.fill_preserve();
        cr.set_source_rgb(0.0, 0.0, 0.0);
        cr.set_line_width(0.01);
        cr.stroke();

        cr.set_source_rgba(1.0, 0.2, 0.2, 0.6);
        cr.arc(0.04, 0.53, 0.02, 0.0, PI * 2.);
        cr.arc(0.27, 0.65, 0.02, 0.0, PI * 2.);
        cr.fill();

        Inhibit(false)
    });

    /*
    let window = Window::new(WindowType::Toplevel);
    window.set_title("GTK+ Program");
    window.set_default_size(350, 70);
    let button = Button::new_with_label("Click me!");
    window.add(&button);
    window.show_all();

    window.connect_delete_event(|_, _| {
        gtk::main_quit();
        Inhibit(false)
    });

    button.connect_clicked(|_| {
        println!("Clicked!");
    });
     */

    gtk::main();
}

pub fn drawable<F>(width: i32, height: i32, draw_fn: F)
    where F: Fn(&DrawingArea, &Context) -> Inhibit + 'static
{
    let window = gtk::Window::new(gtk::WindowType::Toplevel);
    let drawing_area = Box::new(DrawingArea::new)();

    drawing_area.connect_draw(draw_fn);
    drawing_area.connect_motion_notify_event(|_, event_motion| {
        println!("motion notify event: {:?}", event_motion);
        Inhibit(false)
    });

    window.set_default_size(width, height);
    window.connect_delete_event(|_, _| { gtk::main_quit(); Inhibit(false) });
    window.add(&drawing_area);
    window.show_all();
}
