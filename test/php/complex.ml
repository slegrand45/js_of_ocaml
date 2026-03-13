(* Complex example with list processing and exceptions *)
exception ProcessError of string
exception ValidationError of int

let rec process_nested l =
  match l with
  | [] -> 0
  | x :: xs ->
      try
        if x < 0 then raise (ValidationError x)
        else if x > 100 then raise (ProcessError "too large")
        else x + process_nested xs
      with
      | ValidationError v -> 
          print_string "Caught Validation: "; print_int v; print_newline ();
          process_nested xs
      | ProcessError s ->
          print_string "Caught Process: "; print_string s; print_newline ();
          0

let () =
  let data = [10; 20; -5; 30; 150; 40] in
  let res = process_nested data in
  print_string "Final Result: "; print_int res; print_newline ()
