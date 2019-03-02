let rec list_enum n = 
  match n with
    0 -> [0]
  | n -> n::list_enum(n - 1);;

let rec list_mult n =
  match n with
    [] -> 1
    |