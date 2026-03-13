(* Recursive types and Pattern Matching test *)

type 'a list_rec = Nil | Cons of 'a * 'a list_rec

let rec length_rec = function
  | Nil -> 0
  | Cons (_, next) -> 1 + length_rec next

let rec sum_rec = function
  | Nil -> 0
  | Cons (x, next) -> x + sum_rec next

type expr =
  | Const of int
  | Add of expr * expr
  | Mul of expr * expr

let rec eval = function
  | Const n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2

let test_pm_nested () =
  let l = Cons (1, Cons (2, Cons (3, Nil))) in
  print_int (length_rec l); print_string " ";
  print_int (sum_rec l); print_newline ()

let test_pm_expr () =
  let e = Add (Const 10, Mul (Const 5, Const 2)) in
  print_int (eval e); print_newline ()

let () =
  print_string "List Rec: "; test_pm_nested ();
  print_string "Expr: "; test_pm_expr ()
