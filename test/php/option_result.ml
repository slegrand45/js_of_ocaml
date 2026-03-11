(* Option and Result Types Test *)

let describe_option = function
  | None -> 0
  | Some x -> x

let describe_result = function
  | Ok v -> v
  | Error _ -> -1

let safe_div a b =
  if b = 0 then Error "division by zero"
  else Ok (a / b)

let test_option_result () =
  (* 1. Option tests *)
  print_int (describe_option None);
  print_newline ();
  print_int (describe_option (Some 42));
  print_newline ();
  
  (* 2. Result tests *)
  print_int (describe_result (safe_div 10 2));
  print_newline ();
  print_int (describe_result (safe_div 10 0));
  print_newline ()

let () = test_option_result ()
