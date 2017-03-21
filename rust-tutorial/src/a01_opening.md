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

# Lightning Demo

## Demo: sequential web page fetch  { data-transition="fade-out" }

``` {.rust}
fn sequential_web_fetch() {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method

    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
        "http://imgur.com", "http://mozilla.org"];

    for &site in sites { // step through the array...
        let client = Client::new();
        let res = client.get(site).send().unwrap();
        assert_eq!(res.status, hyper::Ok);
        let char_count = res.chars().count();
        println!("site: {} chars: {}", site, char_count);
    }
}
```

. . .

(lets get rid of the Rust-specific pattern binding in `for`; this is not a tutorial)

## Demo: sequential web page fetch  { data-transition="fade" }

```rust
fn sequential_web_fetch() {
    use hyper::{self, Client};
    use std::io::Read; // pulls in `chars` method

    let sites = &["http://www.eff.org/", "http://rust-lang.org/",
        "http://imgur.com", "http://mozilla.org"];

    for site_ref in sites { // step through the array...
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

(on every run, when internet, and sites, available)

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
