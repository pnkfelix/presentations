# some (white) lies: "Rust is just about ownership" { .center }

# "Ownership is intuitive" {.center}

## "Ownership is intuitive"

Let's buy a car

``` {.rust}
let money: Money = bank.withdraw_cash();
let my_new_car: Car = dealership.buy_car(money);
// let second_car = dealership.buy_car(money); // <-- cannot reuse
```

money transferred into `dealership`
and car transferred to us.

. . .

``` {.rust}
my_new_car.drive_to(home);
garage.park(my_new_car);
// my_new_car.drive_to(...) now doesn't work
```

(can't drive car without access to it, e.g. taking it
out of the garage)

``` {.rust}
let my_car = garage.unpark();
my_car.drive_to(work);
```

## Corrected: Ownership is intuitive, except for programmers ... { .center }

. . .

### ... and anyone else who *names* things

## Über Sinn und Bedeutung

("On sense and reference" -- Gottlob Frege, 1892)

If ownership was all we had, details of prior slide seem nonsensical

``` {.rust}
my_new_car.drive_to(home);
```

. . .

Does this transfer `home` into the car?

Do I lose access to my home, just because I drive to it?

. . .

We must distinguish an object itself from ways to name that object

 * Above, `home` cannot be (an owned) `Home`

 * `home` must instead be some kind of *reference* to a `Home`

## a truth: Ownership *is* important {.center}

## Ownership is important

Ownership enables:       which removes:
----------------------   -------------------------------------------
RAII-style destructors   a source of memory leaks (or fd leaks, etc)
no dangling pointers     many resource management bugs
no data races            many multithreading heisenbugs
----------------------   -------------------------------------------

. . .

Ownership also forces one to answer questions like:

> Do I need to take ownership here, accepting the associated
> resource management responsibility? Would temporary
> access suffice?

Good developers ask this already!

Rust forces function signatures to encode explicit answers,
(and they are checked by the compiler).

<div class="notes">
Its worth pointing out that the encoded answer can
include "I leave it up to the caller to decide";
that is what `Cow<'a, T>` is for.
</div>
