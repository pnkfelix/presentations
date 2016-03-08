# Why ...? {.center}

## Why use Rust? { .big_text data-transition="fade-out" }

> - Fast code, low memory footprint
> - Go from bare metal (assembly; C FFI) ...
  <div class="fragment">... to high-level (closures, generic containers) ...</div>
  <div class="fragment">with *zero cost*</div>
> - *Safety*
> - *Parallelism*

<div class="notes">
 * So far, sounds like C++
 * "the UB stops here"
</div>

## Why would you work on Rust?  { .big_text data-transition="fade" }

. . .

Was the previous slide an insufficient answer?

## Why would Mozilla sponsor Rust?   { data-transition="fade" }

>- Hard to prototype research atop C++ code base

>- Rust ⇒ Servo, WebRender

>- Want Rust for next-gen infrastructure (services, IoT)

>- > "Our mission is to ensure the Internet is a global public resource, open and accessible to all. An Internet that truly puts people first, where individuals can shape their own experience and are empowered, safe and independent."

<!-- Abstract Demo Support Code

```rust
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Matrix {
    num_rows: usize, row_len: usize, data: Vec<i32>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Vector {
    data: Vec<i32>,
}
impl Vector {
    pub fn new(n: usize, init: i32) -> Vector { Vector { data: vec![init; n] } }
    pub fn iter(&self) -> ::std::slice::Iter<i32> { self.data.iter() }
    pub fn iter_mut(&mut self) -> ::std::slice::IterMut<i32> { self.data.iter_mut() }
    pub fn par_iter_mut(&mut self) -> ::rayon::par_iter::slice_mut::SliceIterMut<i32> { self.data.par_iter_mut() }
}

pub struct Rows<'a> { row_len: usize, remain: &'a [i32], }

impl<'a> ::std::iter::Iterator for Rows<'a>{
    type Item = &'a [i32];
    #[inline]
    fn next(&mut self) -> Option<&'a [i32]> {
        if self.row_len > self.remain.len() {
            debug_assert_eq!(self.remain.len(), 0);
            return None;
        }
        let (pre, post) = self.remain.split_at(self.row_len);
        self.remain = post;
        return Some(pre);
    }
}

impl Matrix {
    #[inline]
    pub fn row(&self, i: usize) -> &[i32] {
        let offset = i * self.row_len;
        &self.data[offset..(offset + self.row_len)]
    }

    #[inline]
    pub fn rows(&self) -> Rows {
        debug_assert_eq!(self.data.len() % self.row_len, 0);
        Rows { row_len: self.row_len, remain: &self.data[..] }
    }
}

impl ::std::ops::Index<(usize)> for Vector {
    type Output = i32;
    fn index(&self, i: usize) -> &i32 { &self.data[i] }
}
impl ::std::ops::IndexMut<(usize)> for Vector {
    fn index_mut(&mut self, i: usize) -> &mut i32 { &mut self.data[i] }
}
impl ::std::ops::Index<(usize, usize)> for Matrix {
    type Output = i32;
    fn index(&self, (i,j): (usize, usize)) -> &i32 { &self.data[i*self.row_len + j] }
}
```
-->

## Talk plan

>- "Why Rust" Demonstration
>- "Sharing is Great" (... or Terrible)
>- "Ownership is easy" (... or is it?)
>- ___Sharing                        Stuff
   -----------------------------  --------------------------------
   Sharing *capabilities*         (Language stuff)
   Sharing computation *effort*   (Parallelism stuff)
   Sharing *code*                 (Open source distribution stuff)
   -----------------------------  --------------------------------

## Demo: sequential web page fetch  { data-transition="fade-out" }

```rust
fn sequential_web_fetch() {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method

    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
        "http://imgur.com", "http://mozilla.org"];

    for site_ref in sites {
        let site = *site_ref; // (separated for expository purposes)

		{ // (and a separate block, again for expository purposes)
			let client = Client::new();

			let res = client.get(site).send().unwrap();
			assert_eq!(res.status, hyper::Ok);
			let char_count = res.chars().count();
			println!("site: {} chars: {}", site, char_count);
		}
	}
}
```

## Demo: concurrent web page fetch { data-transition="fade-in" }

```rust
fn concurrent_web_fetch() -> Vec<::std::thread::JoinHandle<()>> {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method

    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
        "http://imgur.com", "http://mozilla.org"];
    let mut handles = Vec::new();
    for site_ref in sites {
        let site = *site_ref;
        let handle = ::std::thread::spawn(move || {
            // block code put in closure: ~~~~~~~
            let client = Client::new();

            let res = client.get(site).send().unwrap();
            assert_eq!(res.status, hyper::Ok);
            let char_count = res.chars().count();
            println!("site: {} chars: {}", site, char_count);
        });

        handles.push(handle);
    }

    return handles;
}
```

## Print outs

### Sequential version:

```
site: http://www.eff.org/ chars: 42425
site: http://rust-lang.org/ chars: 16748
site: http://imgur.com chars: 152384
site: http://mozilla.org chars: 63349
```

(on every run with sites available)

### Concurrent version:

```
site: http://imgur.com chars: 152384
site: http://rust-lang.org/ chars: 16748
site: http://mozilla.org chars: 63349
site: http://www.eff.org/ chars: 42425
```
(on at least one run)

<!--
```rust
#[should_panic]
#[test]
fn web_fetch() {
    for j in concurrent_web_fetch() { j.join(); }
    sequential_web_fetch();
    panic!("want to see output");
}
```
-->

## "what is this 'soundness' of which you speak?" {.center}

## Demo: soundness I  { data-transition="fade-out" }

```rust
fn sequential_web_fetch_2() {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method

    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
    //  ~~~~~ `sites`, an array (slice) of strings, is stack-local
        "http://imgur.com", "http://mozilla.org"];

    for site_ref in sites {
    //  ~~~~~~~~ `site_ref` is a *reference to* elem of array.
        let client = Client::new();
        let res = client.get(*site_ref).send().unwrap();
        // moved deref here  ~~~~~~~~~ 
        assert_eq!(res.status, hyper::Ok);
        let char_count = res.chars().count();
        println!("site: {} chars: {}", site_ref, char_count);
    }
}
```

## Demo: soundness II  { data-transition="fade-in" }

```{.rust .compile_error}
fn concurrent_web_fetch_2() -> Vec<::std::thread::JoinHandle<()>> {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method

    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
    //  ~~~~~ `sites`, an array (slice) of strings, is stack-local
        "http://imgur.com", "http://mozilla.org"];
    let mut handles = Vec::new();
    for site_ref in sites {
    //  ~~~~~~~~ `site_ref` still a *reference* into an array
        let handle = ::std::thread::spawn(move || {
            let client = Client::new();
            let res = client.get(*site_ref).send().unwrap();
            // moved deref here  ~~~~~~~~~ 
            assert_eq!(res.status, hyper::Ok);
            let char_count = res.chars().count();
            println!("site: {} chars: {}", site_ref, char_count);
            // Q: will `sites` array still be around when above runs?
        });
        handles.push(handle);
    }
    return handles;
}
```


## Demo: `for` and indexing { data-transition="fade" }

```rust
// Matrix multiply
// A * x = y, where y[i] = Σ A[i,j] * x[j]
fn mult_mat_vec_sqrindexfor(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows;
    let mut y = Vector::new(n, 0);
    for i in 0..n {
        //   ~~~~ yields integers from 0 (inclusive) to n (exclusive)
        for j in 0..n {
            //   ~~~~ ditto
            y[i] += a[(i,j)] * x[j]
        }
    }
    return y;
}
```

<!--
----- - -- - ---
1 2 3   10   140
4 5 6 * 20 = 320
7 8 9   30   500
----- - -- - ---
-->


To ensure *safety*, the indexing operation aboves are bounds checked!

Compiler might remove checks via static analysis (no guarantees)

## Safety: Code was buggy! { data-transition="fade" }

``` {.rust}
// Matrix multiply
// A * x = y, where y[i] = Σ A[i,j] * x[j]
fn mult_mat_vec_sqrindexfor(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows;
    let mut y = Vector::new(n, 0);
    for i in 0..n {
        //   ~~~~
        for j in 0..n {
            //   ~~~~
            y[i] += a[(i,j)] * x[j]
        }
    }
    return y;
}
```

- - - - -- - ----  -------------  - -- --  - - --
1 2 3   10   140                  1 2      1 2 3
4 5 6 * 20 = 320;  but consider:  3 4; or: 4 5 6?
7 8 9   30   500                  5 6
- - - - -- - ----  -------------  - -- --  - - --

## Safety: Code was buggy! { data-transition="fade" }

``` {.rust}
// Matrix multiply
// A * x = y, where y[i] = Σ A[i,j] * x[j]
fn mult_mat_vec_sqrindexfor(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows;
    let mut y = Vector::new(n, 0);
    for i in 0..n {
        //   ~~~~
        for j in 0..n {
            //      ~ (here's our bug: not all matrices are square!)
            y[i] += a[(i,j)] * x[j]
        }
    }
    return y;
}
```

```
panicked at 'index out of bounds: the len is 6 but the index is 6'
stack backtrace:
                           [...]
   8:        0x108dd0690 - panicking::panic_bounds_check::h10583f21138d2ce3cYL
   9:        0x108d42bdf - vec::Vec<T>.Index<usize>::index::h882217903742121516
  10:        0x108d42d02 - a01_opening::Matrix...std..ops..Index<(usize, usize)>::index::h4cea36e77b8429051ma
  11:        0x108d42fcc - a01_opening::mult_mat_vec_sqrindexfor::h1495d7c3020a01aewna
  12:        0x108d4b4c4 - a01_opening::test_nat_tens_sqrforindex_narrow::h4503afde86357094zwa
```

## `for` and indexing (fixed) { data-transition="fade-out" }


```rust
// Matrix multiply
// A * x = y, where y[i] = Σ A[i,j] * x[j]
fn mult_mat_vec_seqindexfor(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows;
    let mut y = Vector::new(n, 0);
    for i in 0..n {
        //   ~~~~
        for j in 0..a.row_len {
            //   ~~~~~~~~~~~~
            y[i] += a[(i,j)] * x[j]
        }
    }
    return y;
}
```

## Nonslide (perf investigation)  { data-transition="fade-out" }

```rust
// Matrix multiply
// A * x = y, where y[i] = Σ A[i,j] * x[j]
fn mult_mat_vec_seq_interim(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows; let mut i = 0;
    let mut y = Vector::new(n, 0);
    for y_i in y.iter_mut() {  // ⇒ yields refs `r` into `y`

        for j in 0..a.row_len {
            //   ~~~~~~~~~~~~
            *y_i += a[(i,j)] * x[j]
        }
        i += 1;
    }
    return y;
}
```

## Abstraction: `for` and iterators { data-transition="fade" }

```rust
// Matrix multiply
// A * x = y, where y[i] = Σ A[i,j] * x[j]
fn mult_mat_vec_seq_iterfor(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows;
    let mut y = Vector::new(n, 0);
    for (i, y_i) in y.iter_mut()    // ⇒ yields refs `r` into `y`
                     .enumerate() { // ⇒ yields (`idx`, `r`) tuples
        for j in 0..a.row_len {

            *y_i += a[(i,j)] * x[j]
        }
    }
    return y;
}
```

`y_i`: example of a *reference*

`y.iter_mut().enumerate()`: an *iterator chain*

iterator chains: lazy traversal; consumed by `for` or ...

<!-- (removing note about indexing, b/c benchmarks are not supporting point)

No indexing ⇒ No checks!

(assuming `a.rows()` is implemented well.)
-->

## Abstraction: iter API for outer loop { data-transition="fade" }
```rust
use itertools::Itertools; // adds `.foreach` to Iterator

fn mult_mat_vec_seq_iterapi(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows;
    let mut y = Vector::new(n, 0);
    y.iter_mut()                   // ⇒ yields pointers `p` into `y`
        .enumerate()               // ⇒ yields (`i`, `p`) tuples
        .foreach(|(i, y_i)| {      // ... finally consumes iterator!
            for j in 0..a.row_len {
                *y_i += a[(i,j)] * x[j]
            }
        });
    return y;
}
```

closure expression syntax: `|arg_1, arg_2, ...| { body ... }`

## Rayon ⇒ *Parallel* outer loop  { data-transition="fade" }
```rust
use rayon::prelude::*; // adds `.par_iter_mut` to array

fn mult_mat_vec_par_iterapi(a: &Matrix, x: &Vector) -> Vector {
    let n = a.num_rows;
    let mut y = Vector::new(n, 0);
    y.par_iter_mut()           // Δ `.par_iter_mut()` here ...
        .enumerate()
        .for_each(|(i, y_i)| { // ... ⇒ these may run in parallel!
            for j in 0..a.row_len {
                *y_i += a[(i,j)] * x[j]
            }
        });
    return y;
}
```

## Safety: Data-race freedom! { data-transition="fade" }

``` {.rust .compile_error}
// rayon prelude also adds `.into_par_iter()` to range objects

fn mult_mat_vec_par_butracy(a: &Matrix, x: &Vector) -> Vector {
    let (m,n) = (a.row_len, a.num_rows);
    let mut y = Vector::new(n, 0);
    (0..m*n).into_par_iter() // ⇐ try reading all cells in parallel!
        .for_each(|ij| {
            let i = ij / m; let j = ij % i;
            y[i] += a[(i,j)] * x[j]
        });
    return y;
}
```

. . .

... but multiple entries for a row affect same entry in `y` ...


``` {.fragment .compile_error}
error: cannot borrow data mutably in a captured outer variable
       in an `Fn` closure
     y[i] += a[(i,j)] * x[j]
     ^
```

<!-- test code

```rust
#[cfg(test)]
fn nat_mat() -> Matrix {
    Matrix { num_rows: 3, row_len: 3,
             data: vec![1,2,3,
                        4,5,6,
                        7,8,9] }
}

#[cfg(test)]
fn nat_mat_narrow() -> Matrix {
    Matrix { num_rows: 3, row_len: 2,
             data: vec![1,2,
                        3,4,
                        5,6] }
}

#[cfg(test)]
fn nat_mat_stout() -> Matrix {
    Matrix { num_rows: 2, row_len: 3,
             data: vec![1,2,3,
                        4,5,6] }
}

#[cfg(test)]
fn tens_vec() -> Vector { Vector { data: vec![10,20,30] } }

#[cfg(test)]
fn big_mat() -> Matrix {
    Matrix { num_rows: 1000, row_len: 1000,
             data: vec![10; 1000000] }
}

#[cfg(test)]
fn big_tens_vec() -> Vector { Vector { data: vec![10; 1000] } }

#[cfg(test)]
fn expect_nat_tens() -> Vector {
    Vector { data: vec![1*10 + 2*20 + 3*30,
                        4*10 + 5*20 + 6*30,
                        7*10 + 8*20 + 9*30] }
}

#[cfg(test)]
fn expect_narrow_tens() -> Vector {
    Vector { data: vec![1*10 + 2*20,
                        3*10 + 4*20,
                        5*10 + 6*20] }
}

#[cfg(test)]
fn expect_stout_tens() -> Vector {
    Vector { data: vec![1*10 + 2*20 + 3*30,
                        4*10 + 5*20 + 6*30] }
}

#[test]
fn test_nat_tens_sqrforindex() {
    assert_eq!(mult_mat_vec_sqrindexfor(&nat_mat(), &tens_vec()),
               expect_nat_tens());
}

#[should_panic]
#[test]
fn test_nat_tens_sqrforindex_narrow() {
    assert_eq!(mult_mat_vec_sqrindexfor(&nat_mat_narrow(), &tens_vec()),
               expect_narrow_tens());
}

#[should_panic]
#[test]
fn test_nat_tens_sqrforindex_stout() {
    assert_eq!(mult_mat_vec_sqrindexfor(&nat_mat_stout(), &tens_vec()),
               expect_stout_tens());
}

#[test]
fn test_nat_tens_seq_forindex() {
    assert_eq!(mult_mat_vec_seqindexfor(&nat_mat(), &tens_vec()),
               expect_nat_tens());
}

#[test]
fn test_nat_tens_seq_forindex_narrow() {
    assert_eq!(mult_mat_vec_seqindexfor(&nat_mat_narrow(), &tens_vec()),
               expect_narrow_tens());
}

#[test]
fn test_nat_tens_seq_forindex_stout() {
    assert_eq!(mult_mat_vec_seqindexfor(&nat_mat_stout(), &tens_vec()),
               expect_stout_tens());
}

#[test]
fn test_nat_tens_seq_interim() {
    assert_eq!(mult_mat_vec_seq_interim(&nat_mat(), &tens_vec()),
               expect_nat_tens());
}

#[test]
fn test_nat_tens_seq_foriter() {
    assert_eq!(mult_mat_vec_seq_iterfor(&nat_mat(), &tens_vec()),
               expect_nat_tens());
}

#[test]
fn test_nat_tens_seq_iter() {
    assert_eq!(mult_mat_vec_seq_iterapi(&nat_mat(), &tens_vec()),
               expect_nat_tens());
}

#[test]
fn test_nat_tens_par_iter() {
    assert_eq!(mult_mat_vec_par_iterapi(&nat_mat(), &tens_vec()),
               expect_nat_tens());
}

#[bench]
fn bench_seq_forindex(b: &mut ::test::Bencher) {
    let m = &big_mat(); let v = &big_tens_vec();
    b.iter(|| mult_mat_vec_seqindexfor(m, v));
}

#[bench]
fn bench_seq_interim(b: &mut ::test::Bencher) {
    let m = &big_mat(); let v = &big_tens_vec();
    b.iter(|| mult_mat_vec_seq_interim(m, v));
}

#[bench]
fn bench_seq_foriter(b: &mut ::test::Bencher) {
    let m = &big_mat(); let v = &big_tens_vec();
    b.iter(|| mult_mat_vec_seq_iterfor(m, v));
}

#[bench]
fn bench_seq_iter(b: &mut ::test::Bencher) {
    let m = &big_mat(); let v = &big_tens_vec();
    b.iter(|| mult_mat_vec_seq_iterapi(m, v));
}

#[bench]
fn bench_par_iter(b: &mut ::test::Bencher) {
    let m = &big_mat(); let v = &big_tens_vec();
    b.iter(|| mult_mat_vec_par_iterapi(m, v));
}
```

-->
