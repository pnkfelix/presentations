# What is Rust? {.center}

## What is Rust?

### New programming language

> - (... and ecosystem, and development community)

### Emphasizing control, safety, and speed

> - (... *especially* when it comes to concurrency)

. . .

### Low-level hacking without fear of segfaults nor data races

> - or more simply: "Hack without Fear"

# Why ...? {.center}

## Why use Rust? { .big_text data-transition="fade-out" }

> - Fast code, low memory footprint
> - Go from bare metal (assembly; C FFI) ...
  <div class="fragment">... to high-level (collections, closures, generic containers) ...</div>
  <div class="fragment">with *zero cost* (no GC, unboxed closures, monomorphization of generics)</div>
> - *Safety* and *Parallelism*

<div class="notes">
 * So far, sounds like C++
 * "the UB stops here"
</div>

## Safety and Parallelism {.center}

### Safety

* No segmentation faults

* No undefined behavior

* No data races

### (Multi-paradigm) Parallelism

  * msg passing via channels

  * shared state via `Arc` and atomics, `Mutex`, etc

  * use native threads... or scoped threads... or work-stealing...

## Why would you (Felix) work on Rust?  { .big_text data-transition="fade" }

. . .

It's awesome!

(Were prior slides really not a sufficient answer?)

. . .

oh, maybe you meant ...

## Why would Mozilla sponsor Rust?   { data-transition="fade" }

>- Hard to prototype research-y browser changes atop C++ code base

>- Rust ⇒ Servo, WebRender

>- Want Rust for next-gen infrastructure (services, IoT)

>- > "Our mission is to ensure the Internet is a global public resource, open and accessible to all. An Internet that truly puts people first, where individuals can shape their own experience and are empowered, safe and independent."

>- "accessible to all"

# Let's Jump In

## A Java Program

```java

```

## A Rust Program

```art
    +------+
    | ♕ /  |
    |  /   |
    | /    |
    |♕-----|
    | \♕   |
    |  \   |
    +------+
```


```rust
type Row = i32;
type Col = usize;

/// Represents a n x n chess board holding only queens, with at most one queen per column.
#[derive(Clone, PartialEq, Eq, Debug)]
struct Board { n: usize, q_row_idx: Vec<Row>, }

impl Board {
    fn empty(n: usize) -> Board { Board { n: n, q_row_idx: vec![] } }

    fn is_complete(&self) -> bool { self.q_row_idx.len() == self.n }

    fn is_legal(&self) -> bool {
        self.q_row_idx.iter().all(|row| *row <= self.n as i32) &&
            self.collision().is_none()
    }

    fn is_illegal(&self) -> bool { !self.is_legal() }

    fn collision(&self) -> Option<((Col, Row), (Col, Row))> {
        for (my_col, &my_row) in self.q_row_idx.iter().enumerate() {
            let mut ne = my_row;
            let mut se = my_row;
            for (idx, &nb_row) in self.q_row_idx[(my_col+1)..].iter().enumerate() {
                let nb_col = my_col + 1 + idx;
                ne += 1;
                se -= 1;
                if my_row == nb_row || ne == nb_row || se == nb_row {
                    return Some(((my_col, my_row), (nb_col, nb_row)));
                }
            }
        }
        return None;
    }
}

#[test]
fn collisions() {
    assert_eq!((Board { n: 4, q_row_idx: vec![0, 0] }).collision(), Some(((0, 0), (1, 0))));
    assert_eq!((Board { n: 4, q_row_idx: vec![0, 2, 0] }).collision(), Some(((0, 0), (2, 0))));
    assert_eq!((Board { n: 4, q_row_idx: vec![1, 3, 1] }).collision(), Some(((0, 1), (2, 1))));
    assert_eq!((Board { n: 4, q_row_idx: vec![0, 1] }).collision(), Some(((0, 0), (1, 1))));
    assert_eq!((Board { n: 4, q_row_idx: vec![1, 3, 2] }).collision(), Some(((1, 3), (2, 2))));
    assert_eq!((Board { n: 5, q_row_idx: vec![2, 3, 1] }).collision(), Some(((0, 2), (1, 3))));
    assert_eq!((Board { n: 5, q_row_idx: vec![2, 4, 0] }).collision(), Some(((0, 2), (2, 0))));
    assert_eq!((Board { n: 4, q_row_idx: vec![1, 3, 0, 2] }).collision(), None);
}
```

Solving n-queens

```rust
struct AllQueenPlacements { b: Board }

impl Iterator for AllQueenPlacements {
    type Item = Board;
    fn next(&mut self) -> Option<Board> {
        if self.b.is_final() {
            return None;
        }
        if self.b.place_new().is_ok() || self.b.adjust_last().is_ok() {
            return Some(self.b.clone());
        }
        // if we got here, then we were not able to place a new
        // piece nor adjust the last one. Pop until we can find
        // something to adjust, and try again.
        loop {
            if self.b.remove_last().is_err() {
                break;
            }
            if self.b.adjust_last().is_ok() {
                break;
            }
        }
        return Some(self.b.clone());
    }
}

struct LegalQueenPlacements { b: Board }

impl Iterator for LegalQueenPlacements {
    type Item = Board;
    fn next(&mut self) -> Option<Board> {
        if self.b.is_final() {
            return None;
        }

        if self.b.place_new().is_ok() {
            if self.b.is_legal() {
                return Some(self.b.clone());
            }
            'adjust_just_placed: loop {
                if self.b.adjust_last().is_ok() {
                    if self.b.is_legal() {
                        return Some(self.b.clone());
                    } else {
                        continue;
                    }
                } else {
                    self.b.remove_last().unwrap();
                    break;
                }
            }
        }
        // If we got here, then there was no location to place a
        // new piece in the neighboring column. Do an iterative
        // `{Adjust* Pop}*` until we can find something legal (or
        // run out of options).
        'adjust_search: loop {
            if self.b.adjust_last().is_ok() {
                if self.b.is_legal() {
                    return Some(self.b.clone());
                } else {
                    continue;
                }
            }
            if self.b.remove_last().is_err() {
                self.b = Board::final_board(self.b.n);
                return None;
            }
        }
    }
}
```

These are the helper methods for the high-level (though
inefficient) iteration.

```rust
impl Board {
    fn final_board(n: usize) -> Board {
        Board { n: n, q_row_idx: ::std::iter::repeat(n as i32 - 1).take(n).collect() }
    }
    fn is_final(&self) -> bool {
        self.q_row_idx.len() == self.n &&
            self.q_row_idx.iter().all(|row| *row == (self.n as i32 - 1))
    }

    fn place_new(&mut self) -> Result<i32, ()> {
        if self.q_row_idx.len() < self.n {
            self.q_row_idx.push(0);
            Ok(0)
        } else {
            Err(())
        }
    }

    fn remove_last(&mut self) -> Result<i32, ()> {
        match self.q_row_idx.pop() {
            Some(x) => Ok(x),
            None => Err(()),
        }
    }

    fn adjust_last(&mut self) -> Result<i32, ()> {
        match self.q_row_idx.last_mut() {
            Some(ref mut l) if **l + 1 < self.n as i32 => {
                **l += 1;
                Ok(**l)
            }
            _ => Err(()),
        }
    }
}
```

Lets try out the iterators.

```rust
#[test]
fn all_2x2() {
    let mut boards = AllQueenPlacements { b: Board::empty(2) };
    let queens: Vec<_> = boards.map(|b| b.q_row_idx).collect();
    assert_eq!(queens, vec![vec![0], vec![0,0], vec![0,1],
                            vec![1], vec![1,0], vec![1,1]]);
}

#[test]
fn all_3x3() {
    let mut boards = AllQueenPlacements { b: Board::empty(3) };
    let queens: Vec<_> = boards.map(|b| b.q_row_idx).collect();
    assert_eq!(queens, vec![vec![0],
                            vec![0,0], vec![0,0,0], vec![0,0,1], vec![0,0,2],
                            vec![0,1], vec![0,1,0], vec![0,1,1], vec![0,1,2],
                            vec![0,2], vec![0,2,0], vec![0,2,1], vec![0,2,2],
                            vec![1],
                            vec![1,0], vec![1,0,0], vec![1,0,1], vec![1,0,2],
                            vec![1,1], vec![1,1,0], vec![1,1,1], vec![1,1,2],
                            vec![1,2], vec![1,2,0], vec![1,2,1], vec![1,2,2],
                            vec![2],
                            vec![2,0], vec![2,0,0], vec![2,0,1], vec![2,0,2],
                            vec![2,1], vec![2,1,0], vec![2,1,1], vec![2,1,2],
                            vec![2,2], vec![2,2,0], vec![2,2,1], vec![2,2,2]]);
}

#[test]
fn all_solved_1x1() {
    let mut boards = AllQueenPlacements { b: Board::empty(1) };
    let queens: Vec<_> = boards
        .filter(|b| b.is_complete() && b.is_legal())
        .map(|b| b.q_row_idx)
        .collect();
    assert_eq!(queens, vec![vec![0]]);
}

#[test]
fn all_solved_3x3() {
    let mut boards = AllQueenPlacements { b: Board::empty(3) };
    let queens: Vec<_> = boards
        .filter(|b| b.is_complete() && b.is_legal())
        .collect();
    assert_eq!(queens, Vec::<Board>::new());
}

#[test]
fn all_solved_4x4() {
    let mut boards = AllQueenPlacements { b: Board::empty(4) };
    let queens: Vec<_> = boards
        .filter(|b| b.is_complete() && b.is_legal())
        .map(|b| b.q_row_idx)
        .collect();
    assert_eq!(queens, vec![vec![1,3,0,2], vec![2,0,3,1]]);
}

#[bench]
fn nqueens_4x4_slow(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = AllQueenPlacements { b: Board::empty(4) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 2);
    });
}

#[bench]
fn nqueens_5x5_slow(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = AllQueenPlacements { b: Board::empty(5) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 10);
    });
}

#[bench]
fn nqueens_6x6_slow(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = AllQueenPlacements { b: Board::empty(6) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 4);
    });
}

#[bench]
fn nqueens_7x7_slow(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = AllQueenPlacements { b: Board::empty(7) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 40);
    });
}

#[test]
fn legal_3x3() {
    let mut boards = LegalQueenPlacements { b: Board::empty(3) };
    let queens: Vec<_> = boards.map(|b| b.q_row_idx).collect();
    assert_eq!(queens, vec![vec![0], vec![0,2],
                            vec![1],
                            vec![2], vec![2,0]]);
}

#[test]
fn legal_4x4() {
    let mut boards = LegalQueenPlacements { b: Board::empty(4) };
    let queens: Vec<_> = boards.map(|b| b.q_row_idx).collect();
    assert_eq!(queens, vec![vec![0], vec![0,2], vec![0,3],   vec![0,3,1],
                            vec![1], vec![1,3], vec![1,3,0], vec![1,3,0,2],
                            vec![2], vec![2,0], vec![2,0,3], vec![2,0,3,1],
                            vec![3], vec![3,0], vec![3,0,2], vec![3,1]]);
}

#[bench]
fn nqueens_4x4_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(4) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 2);
    });
}

#[bench]
fn nqueens_5x5_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(5) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 10);
    });
}

#[bench]
fn nqueens_6x6_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(6) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 4);
    });
}

#[bench]
fn nqueens_7x7_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(7) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 40);
    });
}

#[bench]
fn nqueens_8x8_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(8) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 92);
    });
}

#[bench]
fn nqueens_9x9_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(9) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 352);
    });
}

#[bench]
fn nqueens_10x10_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(10) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 724);
    });
}

#[bench]
fn nqueens_11x11_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(11) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 2680);
    });
}

#[bench]
fn nqueens_12x12_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(12) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 14200);
    });
}

#[bench]
fn nqueens_13x13_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(13) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 73_712);
    });
}

#[bench]
fn nqueens_14x14_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(14) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 365_596);
    });
}

#[bench]
fn nqueens_15x15_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(15) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 2_279_184);
    });
}

#[bench]
fn nqueens_16x16_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(16) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 14_772_512);
    });
}

#[bench]
fn nqueens_17x17_pruned(b: &mut ::test::Bencher) {
    b.iter(|| {
        let mut boards = LegalQueenPlacements { b: Board::empty(17) };
        let num_solns = boards
            .filter(|b| b.is_complete() && b.is_legal())
            .count();
        assert_eq!(num_solns, 95_815_104);
    });
}
```


## Where is Rust now?

 * 1.0 release was back in May 2015

 * Rolling release cycle (up to Rust 1.7 as of March 2nd 2016)

 * Open source from the begining
   `https://github.com/rust-lang/rust/`

 * Open model for future change (RFC process)
   `https://github.com/rust-lang/rfcs/`

 * Awesome developer community
   (~1,000 people in `#rust`, ~250 people in `#rust-internals`, ~1,300 unique commiters to rust.git)

## Talk plan

>- "Why Rust" Demonstration
>- "Ownership is easy" (... or is it?)
>- ___Sharing                        Stuff
   -----------------------------  --------------------------------
   Sharing *capabilities*         (Language stuff)
   Sharing *work*                 (Parallelism stuff)
   Sharing *code*                 (Open source distribution stuff)
   -----------------------------  --------------------------------
