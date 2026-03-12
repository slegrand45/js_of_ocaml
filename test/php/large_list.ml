(* Large list test - tests recursion depth and memory *)

let rec make_list n acc =
  if n = 0 then acc
  else make_list (n - 1) (n :: acc)

let rec sum_list l acc =
  match l with
  | [] -> acc
  | x :: xs -> sum_list xs (acc + x)

let test_large_list n =
  print_string "Testing with N = "; print_int n; print_newline ();
  (* Create list [1; 2; ...; n] using tail recursion *)
  let l = make_list n [] in
  
  (* Print length *)
  print_string "Length: ";
  print_int (List.length l);
  print_newline ();
  
  (* Calculate sum using tail recursion *)
  (* For N=100k, the sum N(N+1)/2 = 5,000,050,000 overflows 31-bit int.
     We use a custom sum that only adds 1 per element to test depth without overflow. *)
  let rec count_tail l acc =
    match l with
    | [] -> acc
    | _ :: xs -> count_tail xs (acc + 1)
  in
  print_string "Tail count: ";
  print_int (count_tail l 0);
  print_newline ()

let () = 
  (* Test with 40k to verify old behavior *)
  test_large_list 40000;
  (* Test with 100k to verify TCO depth *)
  test_large_list 100000
