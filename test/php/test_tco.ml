(* Tail call optimization test *)
let rec count_to n acc =
  if n = 0 then acc
  else count_to (n - 1) (acc + 1)

let () =
  let res = count_to 100000 0 in
  print_int res;
  print_newline ()
