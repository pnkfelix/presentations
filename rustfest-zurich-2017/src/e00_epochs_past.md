# The First of the Three Spirits (8 min)

## IRC snippets

```irc
<epochspast> I am the Ghost of Epochs Past
<pnkscrooge> Long past?
<epochspast> No. Your past.
<pnkscrooge> What business brought you here?
<epochspast> Your welfare!
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

### (RFC 544)

Once had `int` and `uint`. It was confusing. Many renaming initiatives; all met with resistance.

After considering
{`imem`/`umem`, `isize`/`usize`, `offset`/`size`, `intp`/`uintp`}
finally settled on `isize` and `usize` we have today.

> The type is destined for heavy use representing vector/slice indexes, not just addresses.
>
> `usize` (or `size`) may be the best compromise between clarity (since it can be understood equally as container size and memory address space size, and it's used for both) and friendliness (in comparison to something like `umem`).

## 1.0.0-alpha.2: Drop Check

### (RFC 769)

Rust used to not allow this:

```rust
pub struct A<'a, X>(&'a i32, X);
impl<'a, X> Drop for A<'a, X> {
    fn drop(&mut self) { println!("dropping A"); }
}
```

## 1.0 beta: `catch_panic`

### (RFC 1236)

Was not possible to recover from a panic on the thread that caused it.
(Forced monitor/worker split in separate threads, due to the API's in place.)

Initially landed as `catch_panic` in [rust-lang/rust/pull/23651](https://github.com/rust-lang/rust/pull/23651)

Later renamed to `panic::recover`; but today we know it as ...

. . .

`panic::catch_unwind`

it was finally stabilized in *1.9* release (May 2016); much discussion on
[rust-lang/rust#27719](https://github.com/rust-lang/rust/issues/27719)

## 1.0: `mem::forget` is safe

### (RFC 1066)

Represented firming up of collective thinking about what
`unsafe` is meant for.
