open Infer;;

for i=1 to 20 do
  print_int i;
  print_string ". ";
  print_string @@ test i;
  print_string "\n";
done;;