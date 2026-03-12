(* Advanced Recursive Modules Test *)

module rec A : sig
  type t = Leaf of int | Node of B.t
  val size : t -> int
  val create : int -> t
end = struct
  type t = Leaf of int | Node of B.t
  let size = function
    | Leaf _ -> 1
    | Node b -> 1 + B.size b
  let create n = if n <= 0 then Leaf 0 else Node (B.create (n - 1))
end
and B : sig
  type t = A.t list
  val size : t -> int
  val create : int -> t
end = struct
  type t = A.t list
  let size l = List.fold_left (fun acc a -> acc + A.size a) 0 l
  let create n = [A.create n; A.create (n - 1)]
end

let () =
  let tree = A.create 3 in
  print_int (A.size tree);
  print_newline ()

(* Test for Undefined_recursive_module *)
module rec X : sig val x : int end = struct let x = Y.y end
and Y : sig val y : int end = struct let y = X.x end

let () =
  try
    print_int X.x;
    print_newline ()
  with Undefined_recursive_module _ ->
    print_endline "Caught Undefined_recursive_module"
