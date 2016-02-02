## Parallel <span style="visibility: hidden">and Safe</span> { data-transition="fade-out" }

[Rayon]: https://crates.io/crates/rayon/

```rust
fn parallel_qsort(mut vec: &mut [i32]) {
    use {rayon, rand};
    if vec.len() <= 1 { return; }
    let pivot = rand::random::<usize>() % vec.len();
    let mid = vec.partition(pivot);
    let (less, greater) = vec.split_at_mut(mid);
    rayon::join(|| parallel_qsort(less),
                || parallel_qsort(greater)
    );
}
```

(Shout out to [Rayon][].)

<!--
```rust
trait Partition { fn partition(&mut self, at_idx: usize) -> usize; }
impl<'a, X> Partition for &'a mut [X] where X: Ord {
    fn partition(&mut self, at_idx: usize) -> usize {
        self.swap(0, at_idx);
        let (mut i, mut j) = (1, self.len() - 1);
        while i < j {
            if self[i] < self[0] {
                i += 1;
                continue;
            }
            if self[j] > self[0] {
                j -= 1;
                continue;
            }
            // at this point, self[i] >= pivot and self[j] <= pivot.
            self.swap(i, j);
            i += 1;
            j -= 1;
        }
        self.swap(0, i);
        return i;
    }
}
```
-->

. . .

## Parallel and Safe  { data-transition="fade" }

``` {.rust .compile_error}
fn parallel_qsort(mut vec: &mut [i32]) {
    use {rayon, rand};
    if vec.len() <= 1 { return; }
    let pivot = rand::random::<usize>() % vec.len();
    let mid = vec.partition(pivot);
    let (less, greater) = vec.split_at_mut(mid);
    rayon::join(|| parallel_qsort(less),
                || parallel_qsort(less)
    );
}
```

## Parallel and Safe  { data-transition="fade" }

``` {.rust .compile_error}
fn parallel_qsort(mut vec: &mut [i32]) {
    use {rayon, rand};
    if vec.len() <= 1 { return; }
    let pivot = rand::random::<usize>() % vec.len();
    let mid = vec.partition(pivot);
    let (less, greater) = vec.split_at_mut(mid);
    rayon::join(|| parallel_qsort(less),
                || parallel_qsort(less)  // <- BUG: DATA RACE
    );
}
```

```
code.rs:71:17: 71:40 error: closure requires unique access to `less`
                            but it is already borrowed [E0500]
code.rs:71                 || parallel_qsort(less)  // <- BUG: DATA RACE
                           ^~~~~~~~~~~~~~~~~~~~~~~
code.rs:71:35: 71:39 note: borrow occurs due to use of `less`
                           in closure
code.rs:71                 || parallel_qsort(less)  // <- BUG: DATA RACE
                                             ^~~~
code.rs:70:17: 70:40 note: previous borrow of `less` occurs here due
                           to use in closure; the unique capture
                           prevents subsequent moves or borrows of
                           `less` until the borrow ends
code.rs:70     rayon::join(|| parallel_qsort(less),
                                  ^~~~~~~~~~~~~~~~~~~~~~~
```
