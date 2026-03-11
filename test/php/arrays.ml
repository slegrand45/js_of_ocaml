(* Arrays test *)
let test_arrays () =
  let a = [| 1; 2; 3 |] in
  let x = a.(0) in
  a.(1) <- 10;
  x + a.(1)

let () = ignore (test_arrays ())
