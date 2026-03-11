(* Simple Objects test *)
class counter = object
  val mutable count = 0
  method get = count
  method incr = count <- count + 1
end

let () =
  let c = new counter in
  print_int c#get;
  print_newline ();
  c#incr;
  print_int c#get;
  print_newline ()
