# Threads

(Time: 10 minutes.)

  * Goal: Join the threads ([playpen][]) and print out the store with the best price.

  * Extra credit #1: Use channels instead

  * Extra credit #2: Or, instead of channels, use a mutex to compute
    the best price in the parallel threads themselves.

```rust
#![allow(dead_code)]

use std::f32::INFINITY;
use std::sync::Arc;
use std::thread;

struct Store {
    name: String,
    items: Vec<Item>,
}

#[derive(Debug)]
struct Item {
    name: &'static str,
    price: f32,
}

impl Store {
    fn new(name: String) -> Store {
        Store {
            name: name,
            items: vec![],
        }
    }

    fn add_item(&mut self, item: Item) {
        self.items.push(item);
    }

    fn price(&self, item_name: &str) -> f32 {
        for item in &self.items {
            if item.name == item_name {
                return item.price;
            }
        }

        panic!("no such item {:?}", item_name);
    }

    fn total_price(&self, shopping_list: &[&str]) -> f32 {
        shopping_list.iter()
                     .map(|name| self.price(name))
                     .fold(0.0, |a, b| a + b)
    }
}

fn build_stores() -> Vec<Store> {
    let mut stores = vec![];

    let mut store = Store::new(format!("Rustmart"));
    store.add_item(Item { name: "chocolate", price: 5.0 });
    store.add_item(Item { name: "socks", price: 23.0 });
    store.add_item(Item { name: "plush Mozilla dinosaur", price: 13.0 });
    stores.push(store);

    let mut store = Store::new(format!("Rarget"));
    store.add_item(Item { name: "chocolate", price: 2.5 });
    store.add_item(Item { name: "socks", price: 20.0 });
    store.add_item(Item { name: "plush Mozilla dinosaur", price: 20.0 });
    stores.push(store);

    stores
}

fn main() {
    let stores = build_stores();

    let shopping_list = vec!["chocolate", "plush Mozilla dinosaur"];
    let shopping_list = Arc::new(shopping_list);

    let mut handles = vec![];
    for store in stores {
        let shopping_list = shopping_list.clone();
        handles.push(thread::spawn(move || {
            let sum = store.total_price(&shopping_list);
            (store.name, sum)
        }));
    }

    let mut best: Option<Store> = None;
    let mut best_price = INFINITY;

    // Goal: join the threads here!
    // Extra credit: rewrite to use channels or mutexes.

    println!("--> Go to {}!", best.unwrap().name);
}
```
[playpen]: https://play.rust-lang.org/?code=%23%21%5Ballow%28dead_code%29%5D%0A%0Ause%20std%3A%3Af32%3A%3AINFINITY%3B%0Ause%20std%3A%3Async%3A%3AArc%3B%0Ause%20std%3A%3Athread%3B%0A%0Astruct%20Store%20%7B%0A%20%20%20%20name%3A%20String%2C%0A%20%20%20%20items%3A%20Vec%3CItem%3E%2C%0A%7D%0A%0A%23%5Bderive%28Debug%29%5D%0Astruct%20Item%20%7B%0A%20%20%20%20name%3A%20%26%27static%20str%2C%0A%20%20%20%20price%3A%20f32%2C%0A%7D%0A%0Aimpl%20Store%20%7B%0A%20%20%20%20fn%20new%28name%3A%20String%29%20-%3E%20Store%20%7B%0A%20%20%20%20%20%20%20%20Store%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20name%3A%20name%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20items%3A%20vec%21%5B%5D%2C%0A%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%7D%0A%0A%20%20%20%20fn%20add_item%28%26mut%20self%2C%20item%3A%20Item%29%20%7B%0A%20%20%20%20%20%20%20%20self.items.push%28item%29%3B%0A%20%20%20%20%7D%0A%0A%20%20%20%20fn%20price%28%26self%2C%20item_name%3A%20%26str%29%20-%3E%20f32%20%7B%0A%20%20%20%20%20%20%20%20for%20item%20in%20%26self.items%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20item.name%20%3D%3D%20item_name%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20return%20item.price%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%7D%0A%0A%20%20%20%20%20%20%20%20panic%21%28%22no%20such%20item%20%7B%3A%3F%7D%22%2C%20item_name%29%3B%0A%20%20%20%20%7D%0A%0A%20%20%20%20fn%20total_price%28%26self%2C%20shopping_list%3A%20%26%5B%26str%5D%29%20-%3E%20f32%20%7B%0A%20%20%20%20%20%20%20%20shopping_list.iter%28%29%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20.map%28%7Cname%7C%20self.price%28name%29%29%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20.fold%280.0%2C%20%7Ca%2C%20b%7C%20a%20%2B%20b%29%0A%20%20%20%20%7D%0A%7D%0A%0Afn%20build_stores%28%29%20-%3E%20Vec%3CStore%3E%20%7B%0A%20%20%20%20let%20mut%20stores%20%3D%20vec%21%5B%5D%3B%0A%0A%20%20%20%20let%20mut%20store%20%3D%20Store%3A%3Anew%28format%21%28%22Rustmart%22%29%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22chocolate%22%2C%20price%3A%205.0%20%7D%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22socks%22%2C%20price%3A%2023.0%20%7D%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22plush%20Mozilla%20dinosaur%22%2C%20price%3A%2013.0%20%7D%29%3B%0A%20%20%20%20stores.push%28store%29%3B%0A%0A%20%20%20%20let%20mut%20store%20%3D%20Store%3A%3Anew%28format%21%28%22Rarget%22%29%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22chocolate%22%2C%20price%3A%202.5%20%7D%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22socks%22%2C%20price%3A%2020.0%20%7D%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22plush%20Mozilla%20dinosaur%22%2C%20price%3A%2020.0%20%7D%29%3B%0A%20%20%20%20stores.push%28store%29%3B%0A%0A%20%20%20%20stores%0A%7D%0A%0Afn%20main%28%29%20%7B%0A%20%20%20%20let%20stores%20%3D%20build_stores%28%29%3B%0A%0A%20%20%20%20let%20shopping_list%20%3D%20vec%21%5B%22chocolate%22%2C%20%22plush%20Mozilla%20dinosaur%22%5D%3B%0A%20%20%20%20let%20shopping_list%20%3D%20Arc%3A%3Anew%28shopping_list%29%3B%0A%0A%20%20%20%20let%20mut%20handles%20%3D%20vec%21%5B%5D%3B%0A%20%20%20%20for%20store%20in%20stores%20%7B%0A%20%20%20%20%20%20%20%20let%20shopping_list%20%3D%20shopping_list.clone%28%29%3B%0A%20%20%20%20%20%20%20%20handles.push%28thread%3A%3Aspawn%28move%20%7C%7C%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20let%20sum%20%3D%20store.total_price%28%26shopping_list%29%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%28store.name%2C%20sum%29%0A%20%20%20%20%20%20%20%20%7D%29%29%3B%0A%20%20%20%20%7D%0A%0A%20%20%20%20let%20mut%20best%3A%20Option%3CStore%3E%20%3D%20None%3B%0A%20%20%20%20let%20mut%20best_price%20%3D%20INFINITY%3B%0A%0A%20%20%20%20%2F%2F%20Goal%3A%20join%20the%20threads%20here%21%0A%20%20%20%20%2F%2F%20Extra%20credit%3A%20rewrite%20to%20use%20channels%20or%20mutexes.%0A%0A%20%20%20%20println%21%28%22--%3E%20Go%20to%20%7B%7D%21%22%2C%20best.unwrap%28%29.name%29%3B%0A%7D&version=nightly
