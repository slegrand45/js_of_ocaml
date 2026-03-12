(* Advanced Functors Test *)

module type S = sig
  val x : int
  val f : int -> int
end

module type F = sig
  module M : S
  val g : int -> int
end

module Make (M : S) : F with module M = M = struct
  module M = M
  let g y = M.f (M.x + y)
end

module M1 = struct
  let x = 10
  let f y = x + y
end

module F1 = Make(M1)

module type H = sig
  module Functor (M : S) : F
end

module HigherOrder (K : H) (M : S) = struct
  module Applied = K.Functor(M)
  let result = Applied.g 5
end

module K1 = struct
  module Functor (M : S) = Make(M)
end

module Final = HigherOrder(K1)(M1)

let () =
  print_int F1.M.x;
  print_newline ();
  print_int (F1.g 5);
  print_newline ();
  print_int Final.result;
  print_newline ()
