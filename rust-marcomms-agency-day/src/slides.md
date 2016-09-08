## Rust: No Fear Systems Dev {.center}

### Felix Klock (`@pnkfelix`), Mozilla Research
### MarComms Agency Day, Paris; 8 Sept 2016

## What {.center}

Rust: new systems programming language

. . .

Mozilla: building new high-perf. browser technology atop Rust

Servo: Browser research platform

## Demo: WebRender

<iframe width="560" height="315" src="https://www.youtube.com/embed/u0hYIRQRiws" frameborder="0" allowfullscreen></iframe>

[https://youtu.be/u0hYIRQRiws](https://youtu.be/u0hYIRQRiws)

 Browser     Frames/Second
 --------   --------------
 Chrome     15 fps
 Firefox    9 fps
 Safari     5 fp
 Servo      60 fps

# Background {.center}

## Systems

Systems Programmers make:

 * Browsers

 * Operating Systems

 * Game Engines

 * Mobile Apps

Willing to invest great effort to optimize
resource usage (e.g. low memory overhead),
and runtime performance.

## Programming Languages { data-transition="fade-out" .left_align }

So many to choose from

Actionscript, C, C#, C++, Clojure, Fortran, Go, Java, Javascript, Python, Ruby, Visual Basic, ...

## Programming Languages { data-transition="fade-in" .left_align }

So many to choose from

~~Actionscript~~, C, C#, C++, Clojure, Fortran, Go, Java, ~~Javascript~~, ~~Python~~, ~~Ruby~~,  ~~Visual Basic~~, ...

Most not suitable for *systems development* ...

 * hard-to-predict or poor performance

 *

## Programming Languages { data-transition="fade-in" .left_align }

~~So many to choose from~~

~~Actionscript~~, C, ~~C#~~, C++, ~~Clojure~~, Fortran, ~~Go~~, ~~Java~~, ~~Javascript~~, ~~Python~~, ~~Ruby~~, ~~Visual Basic~~, ...

Most not suitable for *systems development* ...

 * hard-to-predict or poor performance

 * reliance on hard-to-manage runtime machinery e.g. "garbage collection" or atomic ref-counting

. . .

... but the languages remaining are *unsafe* ...

 * bugs might crash application, corrupt data, exposure to "pwnage"

. . .

... and complicate utilization of available (multicore) parallelism.

# Programming is Hard {.center}

## { .left_align data-transition="fade-out" }

Example systems code: direct access to bytes in memory

``` {.rust}
buffer = new String("Hello");
```

```art
     .-------------.
buf: | String      |
     | ----------- |
     |     data: --+----.
     | ----------- |    |
     |   length: 5 |    |
     | ----------- |    |  .-----------------------------------------------.
     | capacity: 8 |    '->| 'H' | 'e' | 'l' | 'l' | 'o' |     |     |     |
     '-------------'       '-----------------------------------------------'
```

## { .left_align data-transition="fade-in" }

Example systems code: direct access to bytes in memory

``` {.rust}
buffer = new String("Hello");  interior = &buffer[2..];  assert_eq!(interior[0], 'l');
```

```art
     .-------------.
buf: | String      |
     | ----------- |
     |     data: --+----.
     | ----------- |    |
     |   length: 5 |    |
     | ----------- |    |  .-----------------------------------------------.
     | capacity: 8 |    '->| 'H' | 'e' | 'l' | 'l' | 'o' |     |     |     |
     '-------------'       '-----------------------------------------------'
                                          ^
                                          |
interior: --------------------------------'
```

. . .

``` {.rust}
buffer.append(" all!"); // adds five more bytes
```

. . .

```art
     .-------------.
buf: | String      |
     | ----------- |     .-------------------------------------------------------------------.
     |     data: --+---> | 'H' | 'e' | 'l' | 'l' | 'o' | ' ' | 'a' | 'l' | 'l' | '!' |   |   |
     | ----------- |     '-------------------------------------------------------------------'
     |   length:10 |
     | ----------- |     .-----------------------------------------------.
     | capacity:12 |     | 'H' | 'e' | 'l' | 'l' | 'o' |     |     |     |
     '-------------'     '-----------------------------------------------'
                                        ^  [deleted]
                                        |
interior: --=-=-=-----------------------'   <-- dangling reference!
[deleted]: style="stroke-dasharray: 5,2;"
```

. . .

``` {.rust}
assert_eq!(interior[0], 💣); // 😱
```

## Even Worse: Multithreaded Code { .center }

## {.left_align .separated}

```art
Thread 1                                                                      Thread 2
--------     .-----------.                                                    --------
   buf: ---->| String    |<---------------------------------------------------- :buf
             | --------- |
             |   data: --+--.
             | --------- |  | 
             | length    |  v
             | --------- | .-----------------------------------------------.
             | capacity  | | 'H' | 'e' | 'l' | 'l' | 'o' |     |     |     |
             '-----------' '-----------------------------------------------'
```

Thread 1                                    Thread 2
-------------------------  -------------------------
`buf.append(" left");`       `buf.append(" right");`
-------------------------  -------------------------

Both threads want to update `buf`

What is outcome?

. . .

 * `"Hello left right"` ?

 * `"Hello right left"` ?

. . .

Again, must reallocate underlying buffer

## {.left_align}

```art
Thread 1                                                                      Thread 2
--------     .-----------.                                                    --------
   buf: ---->| String    |<---------------------------------------------------- :buf
             | --------- |
             |   data: --+--.
             | --------- |  | 
             | length    |  v
             | --------- | .-----------------------------------------------.
             | capacity  | | 'H' | 'e' | 'l' | 'l' | 'o' |     |     |     |
             '-----------' '-----------------------------------------------'

   tmp: -.     .--------------------------------------------------------------- :tmp
         |     |
         |     v
         |   .-----------------------------------------------------------------------.
         |   | 'H' | 'e' | 'l' | 'l' | 'o' |     | 'r' | 'i' | 'g' | 'h' | 't' |     |
         |   '-----------------------------------------------------------------------'
         |
         |   .-----------------------------------------------------------------------.
         '-->| 'H' | 'e' | 'l' | 'l' | 'o' |     | 'l' | 'e' | 'f' | 't' |     |     |
             '-----------------------------------------------------------------------'

```

. . .

Data Race! At end could be any of:

 * `"Hello left right"` (Thread 1 finishes before Thread 2 starts)

 * `"Hello right left"` (Thread 2 finishes before Thread 1 starts)

 * `"Hello left"`       (Both start at same time, Thread 2 "wins")

 * `"Hello right"`      (Both start at same time, Thread 1 "wins")

# Enter Rust {.center}

## Objective

Prevent all bugs of this nature

. . .

If your Rust code compiles, it does not have:

 * dangling references

 * buffer overflows

 * crashes (segmentation faults)

 * remote code execution vulnerabilities

 * data race conditions (*big* win for parallel systems)

. . .

No popular systems language offers all of above properties

(only ones from research labs)

## Why Mozilla Invests

>- Hard to prototype changes atop C++ code base (e.g. Firefox today)
>- Rust ⇒ Servo, platform for browswer implementation research
>- Servo ⇒ Parallel CSS matching, [WebRender][] (web content atop GPU)
>- Want Rust for next-gen infrastructure (services, IoT)

[WebRender]: https://air.mozilla.org/bay-area-rust-meetup-february-2016/#@25m50s

. . .

Mozilla's mission statement

> "Our mission is to ensure the Internet is a global public resource, open and accessible to all. An Internet that truly puts people first, where individuals can shape their own experience and are empowered, safe and independent."

# Open and Accessible to All {.center}

## Open and Accessible to All

Original audience was systems programmers: C/C++, focus on enabling interoperation between Rust and C

. . .

But: also have attention from elsewhere

![From 2016 Survey](what_language.png)

Of 1987 Rust users surveyed, 732 left C and C++ unchecked

## { .center }

### Enabling systems programming for those without such experience!

## Open Source, Open Community

Rust has been *open source* since the beginning

Open governance model based on *public RFCs*

We have an *active, amazing community*

## [Language Wars][Business Insider] {.center}

[Business Insider]: http://uk.businessinsider.com/why-coders-get-into-religious-wars-over-programming-languages-2015-6

## Community

We have had a code of conduct from the outset,
stressing inclusiveness, and evolving it as needed

> The Rust community seems to be populated entirely by human beings. I
>  have no idea how this was done. I suspect Graydon Hoare deserves a
>  large share of the credit for leading by example but everyone I have
>  interacted with in the community has been friendly and patient.

http://scattered-thoughts.net/blog/2015/06/04/three-months-of-rust/

```art
---
```

We still need help improving diversity, though; see [2016 survey].

[2016 survey]: https://blog.rust-lang.org/2016/06/30/State-of-Rust-Survey-2016.html

## Projects are pivoting to Rust {.center}

* [Skylight][] swapped Rust for Ruby back in 2014

* [Maidsafe][] went from C++ to Rust in 2015

* Dropbox moved [Diskotech][] from Go to Rust in 2016

  * and ported [Broti][] compression from C to Rust too

* [Friends of Rust][]: organizations using Rust in production

[Friends of Rust]: https://www.rust-lang.org/en-US/friends.html

[Skylight]: http://blog.skylight.io/bending-the-curve-writing-safe-fast-native-gems-with-rust/

[Maidsafe]: https://blog.maidsafe.net/2015/07/01/the-ants-are-coming/

[Diskotech]: http://www.wired.com/2016/03/epic-story-dropboxs-exodus-amazon-cloud-empire/

[Broti]: https://blogs.dropbox.com/tech/2016/06/lossless-compression-with-brotli/

## How to "sell" Rust? {.left_align}

* If you are a C/C++ programmer ...

* ... tired of debugging crashes and/or multithreaded programs

You might want to try Rust

. . .

* If you are a "high-level programmer" (Javascript, Ruby) ...

* ... and you want to expand your mind/skill-set

You might want to try Rust

# What's Next {.center}

## What's Next

### Browser Technology

 * Firefox 48 had the first bits of Rust (media parser)

 * Servo marches on (e.g WebRender 2)

 * Incremental integration with Gecko (e.g. Stylo); want "best of both worlds"

To learn more, contact the Servo team, especially `@pcwalton`

Or try out Servo yourself! [Nightlies][]

[Nightlies]: https://blog.servo.org/2016/06/30/servo-nightlies/

----

### Rust itself

Still establishing vision for 2017; [discussion link][2017 vision discuss]

[2017 vision discuss]: https://internals.rust-lang.org/t/setting-our-vision-for-the-2017-cycle/

Initial proposals:

 * Lower learning curve

 * Faster compiler

 * IDE support

 * Advanced abstractions for concurrency and parallelism

 * More seamless FFI

## Thanks {.center}

![][rust logo]

[rust logo]: rust-logo-blk.svg "Rust Logo" {width="50%"}

[https://www.rust-lang.org/][rust-lang.org]

[rust-lang.org]: https://www.rust-lang.org/
