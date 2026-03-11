(* Polymorphic variants *)
type t = [ `A | `B ]
let test_pv (x : t) = match x with `A -> 1 | `B -> 2
let () = ignore (test_pv `A)
