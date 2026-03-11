(* Recursive Modules Test *)

module rec A : sig
  val f : int -> int
  val g : int -> int
end = struct
  let f n = if n <= 0 then 0 else B.f (n - 1) + 1
  let g n = n + 1
end
and B : sig
  val f : int -> int
end = struct
  let f n = if n <= 0 then 0 else A.g n + 1
end

let () =
  print_int (A.f 10);
  print_newline ()
