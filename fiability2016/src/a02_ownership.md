# Ownership: a metaphor {.center}

## "Ownership is intuitive"  { data-transition="fade-out" }

Let's buy a car

``` {.rust}
let money: Money = bank.withdraw_cash();
let my_new_car: Car = dealership.buy_car(money);
```

``` {.rust .compile_error}
let second_car = dealership.buy_car(money); // <-- cannot reuse
```

money transferred into `dealership`,
and car transferred to us.

## "Ownership is intuitive"  { data-transition="fade" }

Let's buy a car

``` {.rust}
let money: Money = bank.withdraw_cash();
let my_new_car: Car = dealership.buy_car(money);
// let second_car = dealership.buy_car(money); // <-- cannot reuse
```

money transferred into `dealership`,
and car transferred to us.


``` {.rust}
my_new_car.drive_to(home);
garage.park(my_new_car);
```

``` {.rust .compile_error}
my_new_car.drive_to(...) // now doesn't work
```

(can't drive car without access to it, e.g. taking it
out of the garage)

## "Ownership is intuitive"  { data-transition="fade-in" }

Let's buy a car

``` {.rust}
let money: Money = bank.withdraw_cash();
let my_new_car: Car = dealership.buy_car(money);
// let second_car = dealership.buy_car(money); // <-- cannot reuse
```

money transferred into `dealership`,
and car transferred to us.


``` {.rust}
my_new_car.drive_to(home);
garage.park(my_new_car);
// my_new_car.drive_to(...) // now doesn't work
```

(can't drive car without access to it, e.g. taking it
out of the garage)

``` {.rust}
let my_car = garage.unpark();
my_car.drive_to(work);
```

. . .

...reflection time...


## Ownership is important

Ownership enables:       which removes:
----------------------   -------------------------------------------
RAII-style destructors   a source of memory leaks (or fd leaks, etc)
no dangling pointers     many resource management bugs
no data races            many multithreading heisenbugs
----------------------   -------------------------------------------

. . .

> Do I need to take ownership here, accepting the associated
> resource management responsibility? Would temporary
> access suffice?

. . .

Good system developers ask this already!

Rust forces function signatures to encode the answers

(and they are checked by the compiler)

<div class="notes">
Its worth pointing out that the encoded answer can
include "I leave it up to the caller to decide";
that is what `Cow<'a, T>` is for.
</div>


## Problem: Ownership is intuitive, except for programmers ...  { .center }

(copying data like integers, and characters, and .mp3's, is "free")

. . .

### ... and anyone else who *names* things

If ownership were all we had, car-purchase slide seems nonsensical

``` {.rust}
my_new_car.drive_to(home);
```

. . .

Does this transfer `home` into the car?

Do I lose access to my home, just because I drive to it?

. . .

We must distinguish an object itself from ways to name that object

 * `home` must be some kind of *reference* to a `Home`

## So we will need references {.center}

> We can solve any problem by introducing an extra level of indirection

-- David J. Wheeler
