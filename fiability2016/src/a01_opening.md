# Why Rust...? {.center}

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

## Why would Mozilla sponsor Rust?   { data-transition="fade" }

>- Hard to prototype research-y browser changes atop C++ code base

>- Rust ⇒ Servo, WebRender

>- Want Rust for next-gen infrastructure (services, IoT)

## Where is Rust now?

 * 1.0 release was back in May 2015

 * Rolling release cycle (up to Rust 1.8 as of May 2nd 2016)

 * Open source from the begining
   `https://github.com/rust-lang/rust/`

 * Open model for future change (RFC process)
   `https://github.com/rust-lang/rfcs/`

 * Awesome developer community
   (~1,000 people in `#rust`, ~250 people in `#rust-internals`, ~1,300 unique commiters to rust.git)

## Talk plan { .big_text }

>- - "Why Rust"
>- - How we build safe abstractions in Rust: ownership & borrowing
>- - Example 1: Pointers and allocation
>- - Example 2: Concurrency
