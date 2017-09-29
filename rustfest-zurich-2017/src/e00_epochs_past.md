# The First of the Three Spirits <!-- (8 min) -->

## IRC

```irc
<epochspast> I am the Ghost of Epochs Past
<pnkscrooge> Long past?
<epochspast> No. Rust's past.
<pnkscrooge> What business brought you here?
<epochspast> Rust's welfare!
```

## The run-up to 1.0

---------------  ----------------   -----------------
                 Rust 1.0 alpha     9 January 2015
six weeks later  Rust 1.0 alpha.2   20 February 2015
six weeks later  Rust 1.0 beta      3 April 2015
six weeks later  Rust 1.0           15 May 2015
---------------  ----------------   ------------------

What big changes happened back then?

## 1.0 alpha: the `int`-pocalypse 

### ([RFC 544][])

[RFC 544]: https://github.com/nox/rust-rfcs/blob/master/text/0544-rename-int-uint.md

Once had `int` and `uint`. It was confusing.

Many renaming initiatives ([rust-lang/rust#9940][] in 2013, [RFC 464][] in 2014); all met with resistance

Team almost launched 1.0 with `int`/`uint`.

Core team learned important lessons about community input and interaction.

[rust-lang/rust#9940]: https://github.com/rust-lang/rust/issues/9940

[RFC 464]: https://github.com/rust-lang/rfcs/pull/464

. . .

After considering many options
{`imem`/`umem`, `offset`/`size`, `intp`/`uintp`, ...}
finally settled on `isize`/`usize` we have today.

<!-- 

> The type is destined for heavy use representing vector/slice indexes, not just addresses.
>
> `usize` (or `size`) may be the best compromise between clarity (since it can be understood equally as container size and memory address space size, and it's used for both) and friendliness (in comparison to something like `umem`).

-->

## 1.0.0-alpha.2: Drop Check

### ([RFC 769][])

[RFC 769]: https://github.com/nox/rust-rfcs/blob/master/text/0769-sound-generic-drop.md

Rust used to not allow this:

```rust
pub struct A<'a, X>(&'a i32, X);
impl<'a, X> Drop for A<'a, X> {
    fn drop(&mut self) { println!("dropping A"); }
}
```

(`rustc` did not know/require the `&'a i32` be valid while destructor was running)

. . .

`dropck` rules fixed that, ...

... and, yes, caused some code to start being rejected.

---

```irc
<epochspast> You were once willing to accept breakage
<pnkscrooge> "I was young and foolish then; ...
<pnkscrooge> ... I feel old and foolish now."
```

## Footnote on Drop Check

The full and true history is probably a 45 minute talk in itself.

## 1.0 beta: `catch_panic`

### ([PR 23651][rust-lang/rust/pull/23651])

[rust-lang/rust/pull/23651]: https://github.com/rust-lang/rust/pull/23651

[RFC 1236]: https://github.com/nox/rust-rfcs/blob/master/text/1236-stabilize-catch-panic.md

Was not possible to recover from a panic on the thread that caused it.
(Forced monitor/worker split in separate threads, due to the API's in place.)

Initially landed as `catch_panic` in [rust-lang/rust/pull/23651][].

Later renamed to `panic::recover` as part of [RFC 1236][]; but today we know it as ...

. . .

`panic::catch_unwind`

it was finally stabilized in *1.9* release (May 2016); much discussion on
[rust-lang/rust#27719](https://github.com/rust-lang/rust/issues/27719)

## 1.0: `mem::forget` is safe

### ([RFC 1066][])

[RFC 1066]: https://github.com/nox/rust-rfcs/blob/master/text/1066-safe-mem-forget.md

Represented firming up of collective thinking about what
`unsafe` is meant for.

## IRC

```irc
<epochspast> You see? Rust can change!
<pnkscrooge> That was all madness running up to 1.0.
<pnkscrooge> It proves nothing about where we are now.
```
