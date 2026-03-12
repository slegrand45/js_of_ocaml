(* GADTs test *)
type _ t = Int : int t | String : string t
let test_gadt () =
  let x : int t = Int in
  match x with Int -> 42
let () = print_int (test_gadt ()); print_newline ()
