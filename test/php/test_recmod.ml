(* Recursive modules test *)
module rec A : sig
  type t = Leaf of int | Node of B.t
  val size : t -> int
end = struct
  type t = Leaf of int | Node of B.t
  let size = function
    | Leaf _ -> 1
    | Node b -> 1 + B.size b
end
and B : sig
  type t = A.t list
  val size : t -> int
end = struct
  type t = A.t list
  let size l = List.fold_left (fun acc a -> acc + A.size a) 0 l
end

let () =
  let tree = A.Node [A.Leaf 1; A.Node [A.Leaf 2; A.Leaf 3]] in
  print_int (A.size tree); (* 1 + (1 + 1 + 1) = 4 *)
  print_newline ()
