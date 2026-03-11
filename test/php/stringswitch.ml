(* String switch test *)
let test_sswitch s = match s with
  | "hello" -> 1
  | _ -> 0
let () = ignore (test_sswitch "hello")
