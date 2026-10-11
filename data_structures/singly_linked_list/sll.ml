type 'a node = {v: 'a; next: ('a node option) ref}

let new_node v = {v=v; next=(ref None)}

type 'a sll = {head: ('a node option) ref}

let unpeel thing = !thing |> Option.get

let getnext thing = thing.next

let new_sll v = {head=ref (Some {v=v; next=(ref None)})}

let append (v: 'a) (ll: 'a sll) = 
  let rec append_unwrapped = fun (v: 'a) (n: 'a node option ref) ->
    match !n with
    | Some node -> append_unwrapped ~v ~n:(node.next) 
    | None -> n := Some {v=v; next=(ref None)}; {head=ll.head}
  in append_unwrapped (v=v) (n=ll.head)

let size (ll: 'a sll) =
  let rec size_unwrapped = fun (n: 'a node option ref) (acc: int) ->
    match !n with
    | Some node -> size_unwrapped ~n:(node.next) (size+1) 
    | None -> size 
  in size_unwrapped (n=ll.head) (acc=0)

let search (ll: 'a sll) (index: int) = 
  if index < 0 then failwith "search: invalid index";
  let rec search_unwrapped = fun (n: 'a node option ref) (index: int) ->
    match !n with
    | Some node ->
      if index = 0 then node.v
      else search_unwrapped ~n:(node.next) ~index:(index-1)
    | None -> failwith "search: out of bounds" 
  in search_unwrapped (n=ll.head) (index=index)

(*Check if a value `v` exists in the sll `ll`. Returns bool.*)
let exists (v: 'a) (ll: 'a sll) =
  let rec exists_unwrapped = fun (sll: 'a sll) (vlu: 'a) ->
    match !(sll.head) with
    | None -> false
    | Some node ->
      if node.v = vlu then
        true
      else
        exists_unwrapped {head=node.next} v
  in exists_unwrapped ll v

let insert (ll: 'a sll) (v: 'a) (index: int) = 
  if index < 0 then failwith "insert: invalid index";
  match !(ll.head) with
  | Some node -> (*Base case*)
    if index = 0 then
      ll.head := Some {v=v; next=ll.head} 
    else
      let rec insert_unwrapped = fun (n: 'a node option ref) (v: 'a) (index: int) ->
        match !n with
        | Some node ->
          if index = 1 then
            let next = node.next in
              next := Some {v=v; next=n}; {head=ll.head}
          else
            insert_unwrapped (n=node.next) (index=index-1) (v=v)
        | None -> failwith "insert: out of bounds"     
      in insert_unwrapped (n=ll.head) (v=v) (index=index)
  | None -> ll.head := Some (new_node v)

let remove (ll: 'a sll) (index: int) = 
  if index < 0 then failwith "remove: invalid index";
  if index = 0 then (* Base case. *)
    match !(ll.head) with
    | Some h -> ll.head := !(h.next)
    | None -> ()
  else
    let rec remove_unwrapped = fun (n: 'a node option ref) (index: int) ->
      match !n with
      | Some node ->
        if index = 1 then
          let nextnext = Option.get !((Option.get !(node.next)).next) in
            node.next := Some nextnext;
            {head=ll.head} (*return*)
        else
          remove_unwrapped ~n:(node.next) ~index:(index-1)
      | None -> failwith "remove: out of bounds"
    in remove_unwrapped (n=ll.head) (index=index)

let extend (ll_one: 'a sll) (ll_two: 'a sll) = 
  let rec extend_unwrapped = fun (n_one: 'a node option ref) (n_two: 'a node option ref) ->
    match !n_one with
    | Some node -> extend_unwrapped ~n_one:(node.next) ~n_two
    | None -> 
      n_one := !n_two;
      {head=ll_one.head} (*return*)
  in extend_unwrapped (n_one=ll_one.head) (n_two=ll_two.head)

(*Why does this work?*)
let reverse (ll: 'a sll) =
  let head = unpeel ll.head in
  let last = new_node head.v in
  let rec reverse_unwrapped (n: 'a node option ref) (ll: 'a sll) (last: 'a node) in
    let next = n |> unpeel |> getnext 
    and nextnext = n |> unpeel |> getnext |> unpeel |> getnext in
    match !nextnext with
    | None ->
      last.next := !next; (*keep reference to last node*)
    | Some _ ->
      reverse_unwrapped ~n:next ~ll ~last
  in begin
    reverse_unwrapped (n=ll.head) (ll=ll) (last=last);
    {head=ll.head} (*return*) end

(*The second list in the partition has its first elem
on the index argument.*)
let partition (ll: 'a sll) (index: int) =
  if index < 0 then failwith "partition: invalid index";
  let rec partition_unwrapped = fun (ll: 'a sll) (index: int) (acc: 'a sll) ->
    if index = 0 then
      [remove 0 acc; {head=ll.head}] (*making sure we remove that extra ll.head.v at the beginning*)
    else
      partition_unwrapped (remove 0 ll) (index-1) (insert 0 acc (search 0 ll))
  in partition_unwrapped (ll=ll) (index=index) (acc=new_sll (unpeel ll.head).v)
