(* Large list test - tests recursion depth and memory *)

let rec make_list n acc =
  if n = 0 then acc
  else make_list (n - 1) (n :: acc)

let rec sum_list l acc =
  match l with
  | [] -> acc
  | x :: xs -> sum_list xs (acc + x)

let test_large_list () =
  let n = 40000 in
  (* Create list [1; 2; ...; 100000] using tail recursion *)
  let l = make_list n [] in
  
  (* Print length *)
  print_int (List.length l);
  print_newline ();
  
  (* Calculate sum using tail recursion *)
  let s = sum_list l 0 in
  print_int s;
  print_newline ()

let () = test_large_list ()
