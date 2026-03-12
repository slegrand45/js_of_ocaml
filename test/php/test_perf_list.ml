(* Performance test: Large list *)
let rec make_list n acc =
  if n = 0 then acc
  else make_list (n - 1) (n :: acc)

let () =
  let l = make_list 50000 [] in
  let sum = List.fold_left (+) 0 l in
  print_int sum;
  print_newline ()
