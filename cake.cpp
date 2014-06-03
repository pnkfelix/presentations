// Copyright 2014 Felix S. Klock II. See the COPYRIGHT
// file at the top-level directory of this distribution.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#include <iostream>

enum Flavor { chocolate, vanilla };
struct Cake {
    Flavor flavor; int num_slices;
    void eat_slice();
};

Cake birthday_cake(Flavor f, int num_slices);
void eat_at_least(Cake &cake, int const &threshold);
void eat_entire(Cake &cake);
void status(Cake const &cake, std::string when);

void Cake::eat_slice() { this->num_slices -= 1; }

void eat_at_least(Cake &cake, int const &threshold)
{
    int eaten_so_far = 0;
    while (cake.num_slices > 0 && eaten_so_far < threshold) {
        cake.eat_slice();
        eaten_so_far += 1;
    }
}

void eat_entire(Cake &cake) {
    eat_at_least(cake, cake.num_slices);
}

int main () {
    Cake cake = birthday_cake(vanilla, 16);
    status(cake, "at outset");
    eat_at_least(cake, 2);
    status(cake, "after 2");
    eat_entire(cake);
    status(cake, "finally");
}

void status(Cake const &cake, std::string when) {
    std::cout << "cake " << when
              << " has " << cake.num_slices << " slices." << std::endl;
}

Cake birthday_cake(Flavor f, int num_slices) {
    Cake c;
    c.flavor = f;
    c.num_slices = num_slices;
    return c;
}
