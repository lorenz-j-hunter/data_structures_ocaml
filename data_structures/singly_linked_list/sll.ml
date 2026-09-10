type 'a node = {v: 'a; next: ('a node option) ref}

let new_node v = {v=v; next=(ref None)}

type 'a sll = {head: ('a node option) ref}

let new_sll v = {head=ref (Some {v=v; next=(ref None)})}

let rec append_unwrapped = fun ~v ~n ->
  match !n with
  | Some node -> append_unwrapped ~v ~n:(node.next) 
  | None -> n := Some {v=v; next=(ref None)}

let append = fun ~v ~ll ->
  append_unwrapped ~v:v ~n:ll.head

let rec size_unwrapped = fun ~n size ->
  match n with
  | Some node -> size_unwrapped ~n:!(node.next) (size+1) 
  | None -> size 

let size = fun ~ll ->
  size_unwrapped ~n:!(ll.head) 0;