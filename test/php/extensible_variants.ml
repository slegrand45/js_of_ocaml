(* Extensible Variants Test *)

type t = ..

type t += A of int
type t += B of string

let test_basic () =
  print_string "--- Basic Extensible Variants ---"; print_newline ();
  let f x =
    match x with
    | A i -> print_string "Got A: "; print_int i; print_newline ()
    | B s -> print_string "Got B: "; print_string s; print_newline ()
    | _ -> print_string "Unknown"; print_newline ()
  in
  f (A 42);
  f (B "hello");
  f (A 0)

type t += C

let test_nested_extension () =
  print_string "--- Nested Extensions ---"; print_newline ();
  let check x =
    match x with
    | C -> print_string "Got C"; print_newline ()
    | _ -> print_string "Not C"; print_newline ()
  in
  check C;
  check (A 1)

(* Test cross-module style extension using locally defined variants *)
module M = struct
  type expr = ..
  type expr += Int of int
end

type M.expr += Add of M.expr * M.expr

let rec eval = function
  | M.Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | _ -> 0

let test_modules () =
  print_string "--- Cross-Module Extensions ---"; print_newline ();
  let e = Add (M.Int 10, Add (M.Int 20, M.Int 12)) in
  print_int (eval e); print_newline ()

let () =
  test_basic ();
  test_nested_extension ();
  test_modules ()
