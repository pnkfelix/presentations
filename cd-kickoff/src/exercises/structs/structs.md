# Structs

(Time: 10 minutes)

Goal: Implement the `total_price` method ([playpen][]).

```rust
#![allow(dead_code)]

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

    fn price(&self, item_name: &str) -> Option<f32> {
        for item in &self.items {
            if item.name == item_name {
                return Some(item.price);
            }
        }
        None
    }

    fn total_price(&self, shopping_list: &[&str]) -> Option<f32> {
        // Goal: compute the total price of all items in the shopping
        // list. If any of the options are not present, return `None`.
        0.0
    }
}

fn build_store() -> Store {
    let mut store = Store::new(format!("Rustmart"));
    store.add_item(Item { name: "chocolate", price: 5.0 });
    store.add_item(Item { name: "socks", price: 23.0 });
    store.add_item(Item { name: "plush Mozilla dinosaur", price: 13.0 });
    store
}

#[test]
fn total_price() {
    let store = build_store();
    let list = vec!["chocolate", "plush Mozilla dinosaur"];
    assert_eq!(store.total_price(&list), Some(18.0));
}

#[test]
fn total_price_missing() {
    let store = build_store();
    let list = vec!["chocolate", "plush Mozilla dinosaur", "fork and knife"];
    assert_eq!(store.total_price(&list), None);
}
```
[playpen]: https://play.rust-lang.org/?code=%23%21%5Ballow%28dead_code%29%5D%0A%0Astruct%20Store%20%7B%0A%20%20%20%20name%3A%20String%2C%0A%20%20%20%20items%3A%20Vec%3CItem%3E%2C%0A%7D%0A%0A%23%5Bderive%28Debug%29%5D%0Astruct%20Item%20%7B%0A%20%20%20%20name%3A%20%26%27static%20str%2C%0A%20%20%20%20price%3A%20f32%2C%0A%7D%0A%0Aimpl%20Store%20%7B%0A%20%20%20%20fn%20new%28name%3A%20String%29%20-%3E%20Store%20%7B%0A%20%20%20%20%20%20%20%20Store%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20name%3A%20name%2C%0A%20%20%20%20%20%20%20%20%20%20%20%20items%3A%20vec%21%5B%5D%2C%0A%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%7D%0A%0A%20%20%20%20fn%20add_item%28%26mut%20self%2C%20item%3A%20Item%29%20%7B%0A%20%20%20%20%20%20%20%20self.items.push%28item%29%3B%0A%20%20%20%20%7D%0A%0A%20%20%20%20fn%20price%28%26self%2C%20item_name%3A%20%26str%29%20-%3E%20Option%3Cf32%3E%20%7B%0A%20%20%20%20%20%20%20%20for%20item%20in%20%26self.items%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20if%20item.name%20%3D%3D%20item_name%20%7B%0A%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20return%20Some%28item.price%29%3B%0A%20%20%20%20%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20%7D%0A%20%20%20%20%20%20%20%20None%0A%20%20%20%20%7D%0A%0A%20%20%20%20fn%20total_price%28%26self%2C%20shopping_list%3A%20%26%5B%26str%5D%29%20-%3E%20Option%3Cf32%3E%20%7B%0A%20%20%20%20%20%20%20%20%2F%2F%20Goal%3A%20compute%20the%20total%20price%20of%20all%20items%20in%20the%20shopping%0A%20%20%20%20%20%20%20%20%2F%2F%20list.%20If%20any%20of%20the%20options%20are%20not%20present%2C%20return%20%60None%60.%0A%20%20%20%20%20%20%20%200.0%0A%20%20%20%20%7D%0A%7D%0A%0Afn%20build_store%28%29%20-%3E%20Store%20%7B%0A%20%20%20%20let%20mut%20store%20%3D%20Store%3A%3Anew%28format%21%28%22Rustmart%22%29%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22chocolate%22%2C%20price%3A%205.0%20%7D%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22socks%22%2C%20price%3A%2023.0%20%7D%29%3B%0A%20%20%20%20store.add_item%28Item%20%7B%20name%3A%20%22plush%20Mozilla%20dinosaur%22%2C%20price%3A%2013.0%20%7D%29%3B%0A%20%20%20%20store%0A%7D%0A%0A%23%5Btest%5D%0Afn%20total_price%28%29%20%7B%0A%20%20%20%20let%20store%20%3D%20build_store%28%29%3B%0A%20%20%20%20let%20list%20%3D%20vec%21%5B%22chocolate%22%2C%20%22plush%20Mozilla%20dinosaur%22%5D%3B%0A%20%20%20%20assert_eq%21%28store.total_price%28%26list%29%2C%20Some%2818.0%29%29%3B%0A%7D%0A%0A%23%5Btest%5D%0Afn%20total_price_missing%28%29%20%7B%0A%20%20%20%20let%20store%20%3D%20build_store%28%29%3B%0A%20%20%20%20let%20list%20%3D%20vec%21%5B%22chocolate%22%2C%20%22plush%20Mozilla%20dinosaur%22%2C%20%22fork%20and%20knife%22%5D%3B%0A%20%20%20%20assert_eq%21%28store.total_price%28%26list%29%2C%20None%29%3B%0A%7D&version=nightly
