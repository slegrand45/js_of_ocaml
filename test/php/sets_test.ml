module IntSet = Set.Make(Int)
let s = IntSet.empty
let s = IntSet.add 1 s
let s = IntSet.add 2 s
let () =
  print_int (IntSet.cardinal s);
  print_newline ()
