(* Complex example with list processing and exceptions *)
exception ProcessError

let process_list lst = 
  try
    match lst with
    | [] -> 0
    | x :: xs -> x + List.length xs
  with _ -> raise ProcessError

let () =
  print_int (process_list [1; 2; 3]);
  print_newline ();
  print_int (process_list []);
  print_newline ()
