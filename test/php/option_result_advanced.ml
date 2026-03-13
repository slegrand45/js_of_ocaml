(* Option and Result advanced test *)

let test_option_ops () =
  let o = Some 42 in
  let o2 = Option.map (fun x -> x * 2) o in
  let o3 = Option.bind o2 (fun x -> if x > 50 then Some (x - 10) else None) in
  print_string (match o3 with Some v -> string_of_int v | None -> "None"); print_newline ()

let test_result_ops () =
  let r = Ok 10 in
  let r2 = Result.map (fun x -> x + 5) r in
  let r3 = Result.bind r2 (fun x -> if x < 20 then Ok (x * 2) else Error "too big") in
  print_string (match r3 with Ok v -> string_of_int v | Error s -> s); print_newline ()

let () =
  print_string "Option: "; test_option_ops ();
  print_string "Result: "; test_result_ops ()
