(* Complex Pattern Matching test *)
type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

let rec sum_tree = function
  | Leaf n -> n
  | Node (l, r) -> sum_tree l + sum_tree r

type color = Red | Green | Blue | RGB of int * int * int

let int_of_color = function
  | Red -> 1
  | Green -> 2
  | Blue -> 3
  | RGB (r, g, b) -> r + g + b

let () =
  let t = Node (Node (Leaf 1, Leaf 2), Leaf 3) in
  print_int (sum_tree t);
  print_newline ();
  
  print_int (int_of_color Red);
  print_newline ();
  print_int (int_of_color (RGB (10, 20, 30)));
  print_newline ()
