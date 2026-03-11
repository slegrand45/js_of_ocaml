(* Nested Functors Test *)

module type S = sig
  val x : int
end

module type F = sig
  module Apply (Arg : S) : S
end

module MakeOuter (M : S) : F = struct
  module Apply (Arg : S) : S = struct
    let x = M.x + Arg.x
  end
end

module One = struct let x = 1 end
module Two = struct let x = 2 end

module Outer = MakeOuter(One)
module Inner = Outer.Apply(Two)

let () =
  print_int Inner.x;
  print_newline ()
