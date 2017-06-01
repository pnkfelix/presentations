extern crate cursive;

use cursive::Cursive;
use cursive::views::{Dialog, TextView};

fn main() {
    println!("Hello World from TBall");

    let mut siv = Cursive::new();

    siv.add_layer(Dialog::around(TextView::new("Hello Dialog!"))
                  .title("Cursive")
                  .button("Quit", |s| s.quit()));

    siv.run();
}
