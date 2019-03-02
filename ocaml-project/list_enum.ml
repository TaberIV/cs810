let rec list_enum n = 
  match n with
    0 -> [0]
  | n -> n::list_enum(n - 1);;

let rec list_mult intList =
  match intList with
    [] -> 1
  | x::lst -> x * list_mult(lst);;

let rec even l =
  match l with
    [] -> []
  | (x::xs) ->
    if (x mod 2=0)
    then x :: (xs)
    else even xs