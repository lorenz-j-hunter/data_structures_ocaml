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

let rec reverse_unwrapped = fun ~n ~next ~nextnext ~ll ->
  (*helpers*)
  let unpeel = fun thing -> !thing |> Option.get in
  let rec unpeel_times = fun n arg -> begin
    let a = unpeel arg in
    if n = 1 then a else unpeel_times (n-1) a.next end
  (*pattern matching*)
  in match !nextnext with
  | None -> ()
  | Some nextnext_ ->
    let n_next = unpeel n.next in (*n.next*)
    let n_nextnext = unpeel n_next.next in (*n.next.next*)
      reverse_unwrapped ~n:n_next ~next:n_nextnext ~nextnext:nextnext_.next ~ll;
    (nextnext_).next := !(n.next);
    if n = (unpeel ll.head) then
      (*Base case*)
      let wanted_node = unpeel_times 4 ll.head in (*ll.head.next.next.next*)
      let new_head = {v=(wanted_node.v); next=(ref None)} in begin
        new_head.next := Some (unpeel_times 2 ll.head); (*ll.head.next*)
        ll.head := Some new_head;
        (unpeel_times 1 ll.head).next := None; end (*ll.head.next := None*)

let reverse = fun ~ll ->
  (*helpers*)
  let unpeel = fun thing -> !thing |> Option.get in
  let rec unpeel_times = fun n arg -> begin
    let a = unpeel arg in
    if n = 1 then a else unpeel_times (n-1) a.next end
  and access_at n thing = ref (Some (unpeel_times n thing))
  (*base case: l < 3*)
  and l = size ~ll:ll
  and head = unpeel ll.head in
    match (l, l>0) with
    | (3, true) ->
      (*Base case(s)*)
      let wanted_node = unpeel_times 3 ll.head in (*head.next.next*)
      let new_head = {v=wanted_node.v; next=(ref None)} in 
        new_head.next := Some (unpeel head.next);
        ll.head := Some new_head;
        head.next := None;
    | (2, true) ->
      let wanted_node = unpeel_times 2 ll.head in (*head.next*)
      let new_head = {v=wanted_node.v; next=(ref None)} in
        ll.head := Some (unpeel new_head.next);
        head.next := None;
    | (1, true) -> ()
    | (_, true) ->
        reverse_unwrapped
        ~n:(unpeel ll.head)
        ~next:(unpeel_times 2 ll.head)
        ~nextnext:(access_at 3 ll.head)
        ~ll
    | (_, false) -> ()
