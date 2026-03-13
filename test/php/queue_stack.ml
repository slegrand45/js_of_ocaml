(* Queue and Stack test *)

let test_queue () =
  let q = Queue.create () in
  Queue.push 1 q;
  Queue.push 2 q;
  print_int (Queue.pop q); print_string " ";
  print_int (Queue.pop q); print_string " ";
  print_string (if Queue.is_empty q then "empty" else "not empty"); print_newline ()

let test_stack () =
  let s = Stack.create () in
  Stack.push 1 s;
  Stack.push 2 s;
  print_int (Stack.pop s); print_string " ";
  print_int (Stack.pop s); print_string " ";
  print_string (if Stack.is_empty s then "empty" else "not empty"); print_newline ()

let () =
  print_string "Queue: "; test_queue ();
  print_string "Stack: "; test_stack ()
