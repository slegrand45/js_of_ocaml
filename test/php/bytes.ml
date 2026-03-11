(* Bytes test *)
let test_bytes () =
  let b = Bytes.of_string "hello" in
  Bytes.set b 0 'H';
  Bytes.length b
let () = ignore (test_bytes ())
