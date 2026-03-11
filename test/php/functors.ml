(* Real Functors test *)
module type Sig = sig val x : int end
module M = struct let x = 42 end
module F (Arg : Sig) = struct let y = Arg.x + 1 end
module Applied = F(M)

let () =
  print_int Applied.y;
  print_newline ()
