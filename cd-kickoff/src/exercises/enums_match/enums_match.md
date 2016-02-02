```rust
// Adapted from
// http://blog.rust-lang.org/2015/04/17/Enums-match-mutation-and-moves.html

enum BinaryTree<X> where X: Ord {
    Leaf(X),
    Node(Box<BinaryTree<X>>, X, Box<BinaryTree<X>>)
}

fn sample_tree() -> BinaryTree<i32> {
    return BinaryTree::Node(node(leaf(1),
                                 2,
                                 leaf(3)),
                            4,
                            leaf(5));

    type BBT = Box<BinaryTree<i32>>;
    fn leaf(x: i32) -> BBT {
        Box::new(BinaryTree::Leaf(x))
    }
    fn node(l: BBT, x: i32, r: BBT) -> BBT {
        Box::new(BinaryTree::Node(l, x, r))
    }
}

impl BinaryTree<i32> {
    fn weight_by_value(self) -> i32 {
        match self {
            BinaryTree::Leaf(payload) => payload,
            BinaryTree::Node(left, payload, right) =>
                left.weight_by_value() + payload + right.weight_by_value()
        }
    }
}

#[test]
fn tree_demo_1() {
    let tree = sample_tree();
    assert_eq!(tree.weight_by_value(), (1 + 2 + 3) + 4 + 5);
}

// EXERCISE: what happens if you get rid of the occurrences of `ref` in
// `ref left` and `ref right` below? Explain.

// EXERCISE: change to `match *self { ... }` instead of `match self { ... }`
// What else needs fixing?
impl BinaryTree<i32> {
    fn weight(&self) -> i32 {
        match self {
            &BinaryTree::Leaf(payload) => payload,
            &BinaryTree::Node(ref left, payload, ref right) =>
                left.weight() + payload + right.weight()
        }
    }
}

#[test]
fn tree_demo_2() {
    let tree = sample_tree();
    assert_eq!(tree.weight(), (1 + 2 + 3) + 4 + 5);
}

// EXERCISE: Finish the implementation of Tree::grow below. Some of
// the code has been provided, so you can see the `ref mut` variation
// of `ref` (it is analogous to `&mut T`/`&T`.)

impl BinaryTree<i32> {
    /// Increments all of the integers in the tree by one.
    fn grow(&mut self) {
        match *self {
            BinaryTree::Leaf(_) => {
                unimplemented!()
            }
            BinaryTree::Node(ref mut left, _, ref mut right) => {
                unimplemented!();
            }
        }
    }
}

#[test]
fn tree_demo_3() {
    let mut tree = sample_tree();
    assert_eq!(tree.weight(), (1 + 2 + 3) + 4 + 5);
    tree.grow();
    assert_eq!(tree.weight(), (2 + 3 + 4) + 5 + 6);
    tree.grow();
    assert_eq!(tree.weight(), (3 + 4 + 5) + 6 + 7);
}
```
