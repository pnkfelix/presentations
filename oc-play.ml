let show_y = function x ->
  let add3 = function x -> x + 3 in
  let y = add3(3) > 5 in
  y;;

print_string("Hello World\n");;

let myadd (x:int) (y:int) = x + y;;

type intpair = int*int;;

let f (p : intpair) = match p with (l,r) -> l+r;;

print_string "f(2,3) is "; print_int (f (2,3)); print_string " \n";;

type height = Tall | Medium | Short;;

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

