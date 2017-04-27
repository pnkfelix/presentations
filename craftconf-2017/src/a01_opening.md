# What is Rust? {.center}

## What is Rust?

### New programming language

> - (... and ecosystem, and development community)

### Emphasizing control, safety, and speed

> - (... *especially* when it comes to concurrency)

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
 * "but, the UB stops here"
</div>

----

![Safety: No More Undefined Behavior](the_ub_stops_here.png)

## Safety and Parallelism {.center}

### Safety

* No segmentation faults

* No undefined behavior

* No data races

### (Multi-paradigm) Parallelism

  * msg passing via channels

  * shared state (R/W-capabilities controlled via types)

  * use native threads... or scoped threads... or work-stealing...

# Let's Jump In: Safety

##

##### C++

```c++
int gather_negs(std::vector<int> &input, std::vector<int> &out) {
    int count = 0;
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) { out.push_back(-val); count++; }
    }
    return count;
}
```

##### Java

```java
static int gather_negs(ArrayList<Integer> input, ArrayList<Integer> out) {
    int count = 0;
    for (int val: input) {
        if (val < 0) { out.add(-val); count++; }
    }
    return count;
}
```

##### Rust

```rust
fn gather_negs(input: &Vec<i32>, out: &mut Vec<i32>) -> isize {
    let mut count = 0;
    for val_ref in input {
        let val = *val_ref;
        if val < 0 { out.push(-val); count += 1; }
    }
    return count;
}
```

. . .

(silly? well ... slides are ~~strawmen~~ succinct)

## Sample usage of `gather_negs`

<!--
```rust
fn test() {
```
-->

```rust
let values = [-1, 2, 3, -4, -5, -6, -7, -8, -9, 10];
let mut input = Vec::new();
input.extend_from_slice(&values); // put `values` into `input` vector
let mut myvector = Vec::new();
gather_negs(&input, &mut myvector); // gather neg vals of `input` into `myvector`
```

<!--
```rust
}
```
-->

#### can make similar code for C++ and Java, e.g.: {newmargin=0}

```c++
int values[] = { -1, 2, 3, -4, -5, -6, -7, -8, -9, 10};
std::vector<int> input(values, values + sizeof(values) / sizeof(int));
std::vector<int> myvector;
gather_negs(input, myvector); // gather neg vals of `input` into `myvector`
```

```art
                      +-------------------------------+
        input ------->| -1  2  3 -4 -5 -6 -7 -8 -9 10 |
                      +-------------------------------+
(BEFORE)
                      +--+
        myvector ---->|  |
                      +--+
```

. . .

```art
                      +-------------------------------+
        input ------->| -1  2  3 -4 -5 -6 -7 -8 -9 10 |
                      +-------------------------------+
(AFTER)
                      +----------------------+
        myvector ---->|  1  4  5  6  7  8  9 |
                      +----------------------+
```

. . .

##### (Same for Rust, C++ and Java). So why is Rust special?

## More C++ `gather_negs`

<div class="notes">Strawman Example Alert!</div>

```c++
int values[] = { -1, 2, 3, -4, -5, -6, -7, -8, -9, 10};
std::vector<int> input(values, values + sizeof(values) / sizeof(int));
std::vector<int> myvector = input;

gather_negs(myvector, myvector); // gather neg vals of `myvector` into `myvector`
```

How does *this* behave?

. . .

Might expect:

```art
                      +-----------------------------+
(BEFORE)              | -1 2 3 -4 -5 -6 -7 -8 -9 10 |
                      +-----------------------------+

                      +-------------------------------------------+
(AFTER)               | -1 2 3 -4 -5 -6 -7 -8 -9 10 1 4 5 6 7 8 9 |
                      +-------------------------------------------+
```
. . .

##### Reality (for C++) is:

```art

+-----------------------------------------------------------------------+
| -1 2 3 -4 -5 -6 -7 -8 -9 10 1 4 5 6 7 8 9 1 4 5 6 7 8 9 1 4 5 6 7 8 9 |
+-----------------------------------------------------------------------+
```

. . .

##### or:

```art
+-------------------------------------------------------------------------------------------+
| -1 2 3 -4 -5 -6 -7 -8 -9 10 1 4 5 6 7 8 9 1 4 5 6 7 8 9 536870912 536870912 1 4 5 6 7 8 9 |
+-------------------------------------------------------------------------------------------+
```

. . .

##### or:

```
Segmentation fault: 11
```

# C++ and Undefined Behavior

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
>   for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val                 .-------------------------------------------- iter
                            |
                            v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10    |
            len = 10     +----------------------------------+
            cap = 11                                     ^
                                                         |
                                                         |
                                                         |
                                                         |
                                                         |
        myvector.end() ----------------------------------'
```

<div class="notes">Spelling out underlying vector representation
here: three words, a pointer to the data buffer, its current length,
and the buffered capacity, . Note I have chosen capacity here to
accommodate my slides, rather than adhere to any one actual
implementation.</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
>       auto val = *it;
        if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val = -1            .-------------------------------------------- iter
                            |
                            v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10    |
            len = 10     +----------------------------------+
            cap = 11                                     ^
                                                         |
                                                         |
                                                         |
                                                         |
                                                         |
        myvector.end() ----------------------------------'
```

<div class="notes">
First we load the `val` from the iterator
</div>


## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
>       if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val = -1            .-------------------------------------------- iter
                            |
                            v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10    |
            len = 10     +----------------------------------+
            cap = 11                                     ^
                                                         |
                                                         |
                                                         |
                                                         |
                                                         |
        myvector.end() ----------------------------------'
```

<div class="notes">
We check if its negative
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
>           out.push_back(-val);
        }
    }
```

```art
        val = -1            .-------------------------------------------- iter
                            |
                            v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
It is negative, so we negate it and push the result (1) on the end.
</div>


## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
>   for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val                    .----------------------------------------- iter
                               |
                               v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
And we loop. Is the iteration done? No we haven't hit the end.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
>       auto val = *it;
        if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val = 2                .----------------------------------------- iter
                               |
                               v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
So again we load the value.
</div>


## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
>       if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val = 2                .----------------------------------------- iter
                               |
                               v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
2 is non-negative, so we do nothing with it.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
>   for (auto it = input.begin(); it != input.end(); ++it) {
>       auto val = *it;
>       if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val = 3                   .-------------------------------------- iter
                                  |
                                  v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
The case for 3rd elem is just like 2nd, so we'll zoom through it.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
>   for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
                                     .----------------------------------- iter
                                     |
                                     v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
And we loop. Is the iteration done? No we still haven't hit the end.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
>       auto val = *it;
        if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val = -4                     .----------------------------------- iter
                                     |
                                     v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
So we load the value -4.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
>       if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
        val = -4                     .----------------------------------- iter
                                     |
                                     v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
And that's negative.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
>           out.push_back(-val);
        }
    }
```

```art
        val = -4                     .----------------------------------- iter
                                     |
                                     v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |
            len = 11     +----------------------------------+
            cap = 11                                         ^
                                                             |
                                                             |
                                                             |
                                                             |
                                                             |
        myvector.end() --------------------------------------'
```

<div class="notes">
So we push ... but the backing buffer is already full
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
>           out.push_back(-val);
        }
    }
```

```art
        val = -4                     .----------------------------------- iter
                                     |
                                     v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |[old]
            len = 11     +----------------------------------+
            cap = 11
                         +--------------------------------------------------+
                         |                                                  |
                         +--------------------------------------------------+


        myvector.end()
[old]: stroke="blue"
```

<div class="notes">
Lets tease apart what `push_back` will do here. It needs to allocate a
larger buffer.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
>           out.push_back(-val);
        }
    }
```

```art
        val = -4                     .----------------------------------- iter
                                     |
                                     v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |[old]
            len = 11     +----------------------------------+
            cap = 11
                         +--------------------------------------------------+
                         | -1  2  3 -4 -5 -6 -7 -8 -9 10  1                 |
                         +--------------------------------------------------+


        myvector.end()
[old]: stroke="blue"
```

<div class="notes">
Then it copies the current contents to the newly allocated buffer.
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
    for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
>           out.push_back(-val);
        }
    }
```

```art
        val = -4                     .----------------------------------- iter
                                     |
                                     v
        myvector         +----------------------------------+
            buf -------->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1 |[old]
            len = 11     +----------------------------------+
            cap = 11
                         +--------------------------------------------------+
                         | -1  2  3 -4 -5 -6 -7 -8 -9 10  1  4              |
                         +--------------------------------------------------+


        myvector.end()
[old]: stroke="blue"
```

<div class="notes">
We finally push the value `4` onto the end, and we will also need to
adjust `buf`, `len`, and `cap` (to point at the newly allocated
buffer, with its new length and larger capacity).
</div>

## C++: Bugs yield Undefined Behavior { data-transition="fade" }

```c++
>   for (auto it = input.begin(); it != input.end(); ++it) {
        auto val = *it;
        if (val < 0) {
            out.push_back(-val);
        }
    }
```

```art
                                        .-------------------------------- iter
                                        | [iter]
                                        v
        myvector         +----------------------------------+
            buf -----.   : -1  2  3 -4 -5 -6 -7 -8 -9 10  1 :[old]
            len = 12 |   +----------------------------------+
            cap = 16 |
                     |   +--------------------------------------------------+
                     '-->| -1  2  3 -4 -5 -6 -7 -8 -9 10  1  4              |
                         +--------------------------------------------------+
                                                                ^
                                                                |
        myvector.end() -----------------------------------------'
[old]: stroke="blue"
[iter]: stroke="red"
```

<div class="notes">
But `push_back` didn't know about `iter`.  Things have gone terribly
terribly wrong.  (This is called "Iterator Invalidation" in C++. It is
considered programmer error.)

Note typical results are largely dependent on memory allocator
(e.g. how soon old buffer is overwritten or returned to OS, and what its
memory address range is relative to the new buffer).
</div>

## C++ chose speed over safety

Iterator invalidation yields undefined behavior

## Java has Iterator Invalidation too

Analogous code in Java

```java
ArrayList<Integer> myvector = new ArrayList<Integer>(input);
myvector.addAll(Arrays.asList(values));
gather_negs(myvector, myvector);
```

yields `ConcurrentModificationException` at runtime.

. . .

(So Java remains safe; its collection code dynamically checks to
detect iterator invalidation. Programmer's bug is detected at
runtime.)

## Rust

Analogous code in Rust

``` {.rust .compile_error}
let mut myvector = Vec::new();
myvector.extend(values.iter());
gather_negatives(&myvector,
                 &mut myvector  );
```

. . .

yields:

``` {.rust .compile_error}
error: cannot borrow `myvector` as mutable 
  because it is also borrowed as immutable
  --> examples/vex.rs:18:27
   |
17 |     gather_negatives(&myvector,
   |                       -------- immutable borrow occurs here
18 |                      &mut myvector  );
   |                           ^^^^^^^^  - immutable borrow ends here
   |                           |
   |                           mutable borrow occurs here

error: aborting due to previous error
```

at *compile-time*.

## Safe code means no undefined behavior {.left_align}

>- Compiler rejects code that does unsafe things, when feasible

>- Developer must explicitly opt into use of `unsafe` constructs,
   like raw pointer addresses

>- Some things checked at runtime (e.g. array bounds checks); errors
   here yield "panics"

>- No undefined behavior; but unwinds/aborts are allowed

# Let's Jump In: Parallelism

## Example: Sum of (even) numbers in C++

Core function to add the even values in an integer range.

```c++
uint64_t do_partial_sum(int start, int to_do) {
    uint64_t result = 0;
    for (int i = start; i < start + to_do; i++) {
        if (i % 2 == 0) { // avoid compiler reduction to closed form solution
            result += i;
        }
    }
    return result;
}
```

## Parallelize it: Divide and conquer!

```c++
void thread_work(uint64_t *result, int start, int to_do) {
    *result += do_partial_sum(start, to_do);
}

int main(int argc, char *argv[]) {
    uint64_t result;
    /* ... */
    std::vector<std::thread *> threads;
    result = 0;
    for (int start_val = 0, i = 0;
         start_val < max;
         start_val += sum_per_thread, i++) {
        threads.push_back(new std::thread(thread_work,
                                          &result,
                                          start_val,
                                          sum_per_thread));
    }
    for (int i = 0; i < thread_count; i++) {
        threads[i]->join();
    }
    /* ... */
}
```

## Lets try it!

(I know you're aching to see more Rust; we'll get to it!)

## Lets try our C++ code { data-transition="fade" }

threads  average time notes
-------- ------------ -----
1        (TBD)
4        (TBD)
1000     (TBD)
-------- ------------ -----

Four runs, 1 thread

```art
sum of 0 to 1000000000 result: 249999999500000000
time 0.474006s
sum of 0 to 1000000000 result: 249999999500000000
time 0.429937s
sum of 0 to 1000000000 result: 249999999500000000
time 0.430296s
sum of 0 to 1000000000 result: 249999999500000000
time 0.425924s
```

## Lets try our C++ code { data-transition="fade" }

threads  average time notes
-------- ------------ -----
1        0.44004075
4        (TBD)
1000     (TBD)
-------- ------------ -----

Four runs, 1 thread

```art
sum of 0 to 1000000000 result: 249999999500000000
time 0.474006s
sum of 0 to 1000000000 result: 249999999500000000
time 0.429937s
sum of 0 to 1000000000 result: 249999999500000000
time 0.430296s
sum of 0 to 1000000000 result: 249999999500000000
time 0.425924s
```

## Lets try our C++ code { data-transition="fade" }

threads  average time notes
-------- ------------ -----
1        0.44004075
4        (TBD)
1000     (TBD)
-------- ------------ -----


Four runs, 4 threads

```art
sum of 0 to 1000000000 result: 249999999500000000
time 0.154396s
sum of 0 to 1000000000 result: 249999999500000000
time 0.156933s
sum of 0 to 1000000000 result: 249999999500000000
time 0.166546s
sum of 0 to 1000000000 result: 249999999500000000
time 0.153196s
```

## Lets try our C++ code { data-transition="fade" }

threads  average time notes
-------- ------------ -----
1        0.44004075
4        0.15776775
1000     (TBD)
-------- ------------ -----


Four runs, 4 threads

```art
sum of 0 to 1000000000 result: 249999999500000000
time 0.154396s
sum of 0 to 1000000000 result: 249999999500000000
time 0.156933s
sum of 0 to 1000000000 result: 249999999500000000
time 0.166546s
sum of 0 to 1000000000 result: 249999999500000000
time 0.153196s
```

## Lets try our C++ code { data-transition="fade" }

threads  average time notes
-------- ------------ -------
1        0.44004075
4        0.15776775
1000     (TBD)
-------- ------------ -------

Four runs, 1000 threads

```art
sum of 0 to 1000000000 result: 249999999500000000
time 0.134191s
sum of 0 to 1000000000 result: 249999999500000000
time 0.137994s
sum of 0 to 1000000000 result: 249850749500500000
time 0.135118s
sum of 0 to 1000000000 result: 249999999500000000
time 0.134177s
```

## Lets try our C++ code { data-transition="fade" }

threads  average time   notes
-------- -------------- -------
1        ~~0.44004075~~ (but
4        ~~0.15776775~~ its all
1000     ~~0.13537~~    bogus!)
-------- -------------- -------

Four runs, 1000 threads

```art
sum of 0 to 1000000000 result: 249999999500000000
time 0.134191s
sum of 0 to 1000000000 result: 249999999500000000
time 0.137994s
sum of 0 to 1000000000 result: 249850749500500000
time 0.135118s                      -------[uhoh]
sum of 0 to 1000000000 result: 249999999500000000
time 0.134177s
[uhoh]: stroke="red"
```

## Data Race!

## Data Race!

```c++
void thread_work(uint64_t *result, int start, int to_do) {
    *result += do_partial_sum(start, to_do);
 // ~~~~~~~
}

int main(int argc, char *argv[]) {
    uint64_t result;
    /* ... */
    std::vector<std::thread *> threads;
    result = 0;
    for (int start_val = 0, i = 0;
         start_val < max;
         start_val += sum_per_thread, i++) {
        auto result_ptr = &result;
                       // ~~~~~~~
        threads.push_back(new std::thread(thread_work,
                                          result_ptr,
                                          start_val,
                                          sum_per_thread));
    }
    for (int i = 0; i < thread_count; i++) {
        threads[i]->join();
    }
    /* ... */
}
```

## Analogous code in Rust

```rust
fn do_partial_sum(start: isize, to_do: isize) -> u64 {
    let mut result: u64 = 0;
    for i in start..(start + to_do) {
        if i % 2 == 0 {
            result += i as u64;
        }
    }
    return result;
}
```

. . .

or if you prefer

```rust
fn do_partial_sum_alt(start: isize, to_do: isize) -> u64 {
    (start..(start + to_do)).filter(|i| i % 2 == 0).map(|i| i as u64).sum()
}
```

## Parallelized

(Attempt at analogous code, now in Rust.)

<!--
```rust
fn main() {
    let sum_per_thread = 250000000;
    let max = 1000000000;
    let mut result: u64;
```
-->
```{.rust .compile_error}
let mut threads = Vec::new();
result = 0;
::crossbeam::scope(|scope| {
    let mut start_val = 0;
    while start_val < max {
        let result_ptr = &mut result;
        threads.push(scope.spawn(move || {
            *result_ptr += do_partial_sum(start_val, sum_per_thread);
        }));
        start_val += sum_per_thread;
    }
    for thread in threads {
        thread.join();
    }
});
```
<!--
```rust
}
```
-->

## No data races allowed!

```
error: cannot borrow `*result` as mutable more than once at a time
   --> src/a01_opening.rs:963:31
    |
960 | ::crossbeam::scope(|scope| {
...
962 |     while start_val < max {
    |
963 |         let result_ptr = &mut result;
    |                               ^^^^^^
    |                               |
    |                               second mutable borrow occurs here
    |                               first mutable borrow occurs here
...
972 | });
    | - first borrow ends here

error: aborting due to previous error
```

## Eliminating the race {.left_align}

* In both C++ and in Rust, you can easily eliminate the data race

 >- common approach: can wrap a mutex around `result`
 >- better: use atomic operation (e.g. compare-and-swap, fetch-and-add)
 >- better still: use separate result receiver for each thread, then add
    those up at end.

* Point is: Rust *forces* you to resolve this bug

* C++ allows silent, scheduler dependent failure (which may arise only rarely)

# Why ...? (continued) {.center}


## Why would Mozilla sponsor Rust?

>- Hard to prototype research-y browser changes atop C++ code base

>- Rust ⇒ Servo, WebRender

>- Want Rust for next-gen infrastructure (services, IoT)

>- > "Our mission is to ensure the Internet is a global public resource, open and accessible to all. An Internet that truly puts people first, where individuals can shape their own experience and are empowered, safe and independent."

>- "accessible to all": IMO, Rust may "bring system programming to the masses"

# Where is Rust now?

## Where is Rust now?

 * 1.0 release was back in May 2015

 * Rolling release cycle (up to Rust 1.16 as of March 16nd 2017)

 * Open source from the begining
   `https://github.com/rust-lang/rust/`

 * Open model for future change (RFC process)
   `https://github.com/rust-lang/rfcs/`

 * Awesome developer community
   (~1,400 people in `#rust`, ~300 people in `#rust-internals`, ~1,900 unique commiters to rust.git)

# Talk plan

## Talk plan

>- Demonstration, "Why Rust"
>- "Ownership is easy" (... or is it?)
>- Sharing (Data) between Threads
>- Sharing (Libraries) between People
