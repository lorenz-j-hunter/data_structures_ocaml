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
  match !n with
  | Some node -> size_unwrapped ~n:(node.next) (size+1) 
  | None -> size 

let size = fun ~ll ->
  size_unwrapped ~n:(ll.head) 0

let rec search_unwrapped = fun ~n ~index ->
  match !n with
  | Some node ->
    if index = 0 then node.v
    else search_unwrapped ~n:(node.next) ~index:(index-1)
  | None -> failwith "search: out of bounds" 

let search = fun ~ll ~index ->
  if index < 0 then failwith "search: invalid index";
  search_unwrapped ~n:(ll.head) ~index:index

let rec insert_unwrapped = fun ~n ~index ~v ->
  match !n with
  | Some node ->
    if index = 1 then
      let next = node.next in
        next := Some {v=v; next=n}
    else
      insert_unwrapped ~n:(node.next) ~index:(index-1) ~v:v
  | None -> failwith "insert: out of bounds"

let insert = fun ~ll ~v ~index ->
  if index < 0 then failwith "insert: invalid index";
  match !(ll.head) with
  | Some node -> (*Base case*)
    if index = 0 then
      ll.head := Some {v=v; next=ll.head} 
    else insert_unwrapped ~n:(ll.head) ~v:v ~index:index
  | None -> ll.head := Some (new_node v)

let rec remove_unwrapped = fun ~n ~index ->
  match !n with
  | Some node ->
    if index = 1 then
      let nextnext = Option.get !((Option.get !(node.next)).next) in
        node.next := Some nextnext
    else
      remove_unwrapped ~n:(node.next) ~index:(index-1)
  | None -> failwith "remove: out of bounds"

let remove = fun ~ll ~index ->
  if index < 0 then failwith "remove: invalid index";
  if index = 0 then (* Base case. *)
    match !(ll.head) with
    | Some h -> ll.head := !(h.next)
    | None -> ()
  else
    remove_unwrapped ~n:(ll.head) ~index:index

let rec extend_unwrapped = fun ~n_one ~n_two ->
  match !n_one with
  | Some node -> extend_unwrapped ~n_one:(node.next) ~n_two
  | None -> n_one := !n_two

let extend = fun ~ll_one ~ll_two ->
  extend_unwrapped ~n_one:(ll_one.head) ~n_two:(ll_two.head)

let rec reverse_unwrapped ~n ~next ~nextnext ~last ~ll =
  match nextnext with
  | None -> last.next := !next (*keep reference to last node*) 
  | Some nextnext_ ->
    let unpeel thing = !thing |> Option.get in
    let next = unpeel n.next
    and nextnext = unpeel next.next 
    and nextnextnext = !nextnext_.next
    in begin reverse_unwrapped ~n:next ~next:nextnext ~nextnext:nextnextnext ~last ~ll;
      if n = unpeel ll.head then begin (*base case*)
        (*perform reversal*)
        ref nextnextnext := next;
        ref nextnext := n;
        ll.head := !last.next; end
      else (*recursive case*)
        ref nextnextnext := nextnext; end

let reverse ll =
  let unpeel thing = !thing |> Option.get
  and getnext thing = thing.next in
  let head = unpeel ll.head
  and next = ll.head |> unpeel |> getnext |> unpeel (*unpeel head.next*)
  and nextnext = ll.head |> unpeel |> getnext |> unpeel |> getnext |> unpeel (*unpeel head.next.next*)
  and last = new_node head.v in
  in reverse_unwrapped ~n:head ~next:next ~nextnext:(Some nextnext) ~last:last ~ll:ll