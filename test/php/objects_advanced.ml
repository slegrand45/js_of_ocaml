(* Advanced Objects and Classes Test *)

(* 1. Simple Inheritance & Initializers *)
class base (x : int) = object
  val mutable v = x
  initializer print_string "Base initialized"; print_newline ()
  method get_v = v
  method set_v x = v <- x
end

class derived (x : int) (y : int) = object
  inherit base x as super
  val mutable w = y
  initializer print_string "Derived initialized"; print_newline ()
  method get_w = w
  method get_all = (super#get_v, w)
end

let test_inheritance () =
  print_string "--- Inheritance & Initializers ---"; print_newline ();
  let d = new derived 1 2 in
  let (v, w) = d#get_all in
  print_int v; print_string ", "; print_int w; print_newline ();
  d#set_v 10;
  print_int d#get_v; print_newline ()

(* 2. Virtual Methods & Abstract Classes *)
class virtual abstract_base = object
  method virtual greet : string -> unit
end

class concrete_greet prefix = object
  inherit abstract_base
  method greet name = print_string (prefix ^ " " ^ name); print_newline ()
end

let test_virtual () =
  print_string "--- Virtual Methods ---"; print_newline ();
  let g = new concrete_greet "Hello" in
  g#greet "OCaml"

(* 3. Multiple Inheritance (Mixins) *)
class printable = object
  method print = print_string "printable"; print_newline ()
end

class serializable = object
  method serialize = print_string "serializable"; print_newline ()
end

class both = object
  inherit printable
  inherit serializable
end

let test_mixins () =
  print_string "--- Multiple Mixins ---"; print_newline ();
  let b = new both in
  b#print;
  b#serialize

(* 4. Private Methods *)
class private_meth = object (self)
  method private secret = 42
  method public_access = self#secret
end

let test_private () =
  print_string "--- Private Methods ---"; print_newline ();
  let p = new private_meth in
  print_int p#public_access; print_newline ()

let () =
  test_inheritance ();
  test_virtual ();
  test_mixins ();
  test_private ()
