(* Complex GADTs Test *)

(* 1. Existential Types *)
type any_showable = Show : 'a * ('a -> string) -> any_showable

let show_any (Show (x, show)) = show x

let test_existential () =
  print_string "--- Existential GADTs ---"; print_newline ();
  let s1 = Show (42, string_of_int) in
  let s2 = Show ("hello", fun x -> x) in
  print_string (show_any s1); print_newline ();
  print_string (show_any s2); print_newline ()

(* 2. Recursive GADT (Typed Expressions) *)
type _ term =
  | Int : int -> int term
  | Bool : bool -> bool term
  | Add : int term * int term -> int term
  | IsZero : int term -> bool term
  | If : bool term * 'a term * 'a term -> 'a term

let rec eval : type a. a term -> a = function
  | Int n -> n
  | Bool b -> b
  | Add (e1, e2) -> eval e1 + eval e2
  | IsZero e -> (eval e) = 0
  | If (c, t, e) -> if eval c then eval t else eval e

let test_recursive () =
  print_string "--- Recursive GADTs ---"; print_newline ();
  let expr = If (IsZero (Add (Int 1, Int (-1))), Int 100, Int 200) in
  print_int (eval expr); print_newline ()

(* 3. Equality Witness *)
type (_, _) eq = Refl : ('a, 'a) eq

let cast : type a b. (a, b) eq -> a -> b = fun Refl x -> x

let test_equality () =
  print_string "--- Equality Witness ---"; print_newline ();
  let x = cast Refl 42 in
  print_int x; print_newline ()

(* 4. Deep Pattern Matching with GADTs *)
type _ t =
  | I : int -> int t
  | S : string -> string t
  | P : 'a t * 'b t -> ('a * 'b) t

let rec deep_match : type a. a t -> unit = function
  | I i -> print_int i
  | S s -> print_string s
  | P (I i, S s) -> print_int i; print_string ", "; print_string s
  | P (a, b) -> deep_match a; print_string "; "; deep_match b

let test_deep_match () =
  print_string "--- Deep Matching ---"; print_newline ();
  deep_match (P (I 10, S "test")); print_newline ();
  deep_match (P (S "nested", P (I 1, I 2))); print_newline ()

let () =
  test_existential ();
  test_recursive ();
  test_equality ();
  test_deep_match ()
