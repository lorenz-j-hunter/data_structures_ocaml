type 'a node = {v: 'a; next: 'a node option}

let new_node v = {v=v; next=None}

type 'a sll = {head: 'a node option}

let new_sll v = {head=Some {v=v; next=None}}

let rec append_unwrapped = fun ~v ~n ->
  match n with
  | Some node -> append_unwrapped ~v ~n:node.next 
  | None -> let modified = ref n in modified := Some {v=v; next=None}

let append = fun ~v ~ll ->
  append_unwrapped ~v:v ~n:ll.head

let rec size_unwrapped = fun ~n size ->
  match n with
  | Some node -> size_unwrapped ~n:node.next (size+1) 
  | None -> size 

let size = fun ~ll ->
  size_unwrapped ~n:ll.head 0;