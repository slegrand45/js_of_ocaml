(* Array iteration functions test *)

let test_iter_map () =
  let a = [| 1; 2; 3; 4; 5 |] in
  let b = Array.map (fun x -> x * x) a in
  Array.iter (fun x -> print_int x; print_string " ") b;
  print_newline ()

let test_fold () =
  let a = [| 1; 2; 3; 4; 5 |] in
  let sum = Array.fold_left (+) 0 a in
  print_int sum; print_newline ()

let test_sort () =
  let a = [| 5; 3; 1; 4; 2 |] in
  Array.sort compare a;
  Array.iter (fun x -> print_int x; print_string " ") a;
  print_newline ()

let () =
  print_string "Map/Iter: "; test_iter_map ();
  print_string "Fold: "; test_fold ();
  print_string "Sort: "; test_sort ()
