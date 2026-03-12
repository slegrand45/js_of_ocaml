(* String switch test *)
let test_sswitch s = match s with
  | "hello" -> 1
  | _ -> 0
let () = print_int (test_sswitch "hello"); print_newline ()
