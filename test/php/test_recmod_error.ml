(* Undefined recursive module test *)
module rec X : sig val get_y : unit -> int end = struct
  let get_y () = Y.y
end
and Y : sig val y : int end = struct
  let y = X.get_y ()
end

let () =
  try
    print_int Y.y;
    print_newline ()
  with Undefined_recursive_module _ ->
    print_endline "Caught Undefined_recursive_module"
