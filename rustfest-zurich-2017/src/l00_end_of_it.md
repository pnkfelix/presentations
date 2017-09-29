# The End of It <!-- (15 min) -->

## Acccepting Change

```irc
<pnkscrooge> Cratchit, I now see: *life* is change!
<acrichto> ... again, that's not my name.
```

All recognized that this was a *new* Scrooge, reformed in his
thinking (though not his memory)

Fundamentally different from the man from a year before

 . . .

One might even call him: a Scrooge for 2018

----


```irc
<aturon> what has pnkscrooge been ranting about?
<aturon> we aren't going to break any code
```

## What's today, my fine fellow? {.left_align}

Already have:

 * nightly/stable split
 * rapid release cycle, and
 * support for deprecation of outdated API's.

Allow Rust to evolve over time, while keeping old code working

. . .

However:

 * insufficient for some kinds of changes.

 * allowed changes come at their own pace; Rust evolves gradually and
   each release is "a somewhat ho hum affair." 

## Epochery

### ([RFC 2052][])

[RFC 2052]: https://github.com/rust-lang/rfcs/blob/master/text/2052-epochs.md

Rust project will declare an *epoch* every two or three years

Each epoch provides

 * coherent set of new features and APIs, stabilized since last epoch

 * where tooling, docs and stdlib are updated to mesh with new features

## Having compatibility cake and eating it too

Backwards incompatible changes only available if crate *opts into*
epoch in its `Cargo.toml`, e.g.

``` {.rust}
epoch = "2019"
```

Otherwise tools assume 2015 epoch.

. . .

Rust compilers must support all extant epochs

Crate graph may involve many epochs at once; must be able to link code and preserve semantics.

Each epoch can introduce hard error *only if* preceding epoch issued a
*deprecation* warning for same input.

. . .

### Epochs do not split ecosystem, nor do they break existing code.

## Epochs let us *consider* new changes {.left_align}

* `catch` (currently implemented as `do catch`)

* deduce module hierarchy from filesystem

* `dyn Trait`

* visibility (`crate fn foo` instead of `pub(crate) fn foo`)

. . .

Note: epochs will largely focus on syntactic changes

 * syntax change *can* enable semantic change
 * still need to link old and new code together with coherent result
 * how you read/write code may change, but mental model should not

## impl Future for Rust

None of this can happen without our awesome community!

We're encouraging everyone (at *every* level of knowledge) to get
involved during our 2017 `impl`-period. This is the perfect time to
dive in and get some mentorship on a bug.