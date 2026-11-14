type 'a node = {v: 'a; next: ('a node option) ref}

let new_node v = {v=v; next=(ref None)}

type 'a sll = {head: ('a node option) ref}

let unpeel thing = !thing |> Option.get

let getnext thing = thing.next

let getv thing = (!thing |> Option.get).v

let new_sll v = {head=ref (Some {v=v; next=(ref None)})}

let append (v: 'a) (ll: 'a sll) = 
  let rec append_unwrapped = fun ~(v: 'a) ~(n: 'a node option ref) ->
    match !n with
    | Some node -> append_unwrapped ~v:v ~n:node.next
    | None -> n := Some {v=v; next=(ref None)}; {head=ll.head}
  in append_unwrapped ~v:v ~n:ll.head

let size (ll: 'a sll) =
  let rec size_unwrapped = fun ~(n: 'a node option ref) ~(acc: int) ->
    match !n with
    | Some node -> size_unwrapped ~n:node.next ~acc:(acc+1) 
    | None -> acc 
  in size_unwrapped ~n:ll.head ~acc:0

let search (ll: 'a sll) (index: int) = 
  if index < 0 then failwith "search: invalid index";
  let rec search_unwrapped = fun ~(n: 'a node option ref) ~(index: int) ->
    match !n with
    | Some node ->
      if index = 0 then node.v
      else search_unwrapped ~n:(node.next) ~index:(index-1)
    | None -> failwith "search: out of bounds" 
  in search_unwrapped ~n:ll.head ~index:index

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
    if index = 0 then begin
      let cycle_preventer = new_node (ll.head |> getv) in
        cycle_preventer.next := ll.head |> unpeel |> getnext |> (!); (*= next node*)
        let ret: 'a sll = {head={v=v; next=cycle_preventer |> Option.some |> ref} |> Option.some |> ref} in ret end
    else
      let rec insert_unwrapped = fun ~(n: 'a node option ref) ~(v: 'a) ~(index: int) ->
        match !n with
        | Some node ->
          if index = 1 then
            let next = node.next in
              next := Some {v=v; next=n}; {head=ll.head}
          else
            insert_unwrapped ~n:node.next ~index:(index-1) ~v:v
        | None -> failwith "insert: out of bounds"     
      in insert_unwrapped ~n:ll.head ~v:v ~index:index
  | None -> {head= new_node v |> Option.some |> ref}

let remove (ll: 'a sll) (index: int) = 
  if index < 0 then failwith "remove: invalid index";
  if index >= size ll then failwith "remove: invalid index";
  match index with
  | 0 ->
    ll.head := ll.head |> unpeel |> getnext |> (!);
    {head=ll.head}
  | 1 ->
    let next = ll.head |> unpeel |> getnext and nextnext = ll.head |> unpeel |> getnext |> unpeel |> getnext |> (!) in
      next := nextnext;
      {head=ll.head}
  | _ ->
    let rec remove_unwrapped = fun ~(n: 'a node option ref) ~(index: int) ->
      match n |> unpeel |> getnext |> unpeel |> getnext |> (!) with (*=nextnext*)
      | None ->
        let next = n |> unpeel |> getnext in
          next := None;
        {head=ll.head} (*return*)
      | Some _ ->
        let next = n |> unpeel |> getnext and nextnext = n |> unpeel |> getnext |> unpeel |> getnext |> (!) in
          if index = 1 then begin
            next := nextnext;
            {head=ll.head} end
          else
            remove_unwrapped ~n:next ~index:(index-1)
    in remove_unwrapped ~n:ll.head ~index:index

let extend (ll_one: 'a sll) (ll_two: 'a sll) = 
  let rec extend_unwrapped = fun ~(n_one: 'a node option ref) ~(n_two: 'a node option ref) ->
    match !n_one with
    | Some node ->
      extend_unwrapped  ~n_one:(node.next) ~n_two
    | None -> 
      n_one := !n_two;
      {head=ll_one.head} (*return*)
  in extend_unwrapped ~n_one:ll_one.head ~n_two:ll_two.head 

let reverse (ll: 'a sll) =
  let origin = {head=ref None} in
  let rec reverse_unwrapped = fun ~(n: 'a node option ref) ~(p: 'a node option ref) -> begin
    let next = n |> unpeel |> getnext
    and nextnext = n |> unpeel |> getnext |> unpeel |> getnext in
    match !nextnext with
    | None ->
      origin.head := new_node (getv next) |> Option.some;
      let o = unpeel origin.head in
        o.next := new_node (getv n) |> Option.some;
        p := o.next |> (!);
    | Some _ ->
      reverse_unwrapped ~n:next ~p;
      let o = unpeel p in (*keep a pointer on the last node of origin.*)
        o.next := new_node (getv n) |> Option.some;
        p := o.next |> (!);
  end in begin
    match size ll with
    | 0 -> origin  
    | 1 -> ll
    | 2 ->
      let first = search ll 1 and second = search ll 0 and ret = {head=ref None} in
        append first ret |> append second
    | _ ->
      let p = new_node (getv ll.head) |> Option.some |> ref in
      let _ = reverse_unwrapped ~n:ll.head ~p in
        origin end

(*The second list in the partition has its first elem
on the index argument.*)
let partition (ll: 'a sll) (i_arg: int) =
  (*base case*)
  if size ll <= i_arg then failwith "partition: invalid argument";
  let first: 'a sll ref = ref {head=ref None} and second: 'a sll ref = ref {head=ref None} in
  let rec partition_unwrapped = fun ~(n: 'a node option ref) ~(i: int) ->
    let next = n |> unpeel |> getnext in
      match !next with
      | None ->
        if i >= i_arg then begin
          let v = getv n in (second := append v !second); end
      | Some _ -> begin
        if i >= i_arg then begin
          let v = getv n in (second := append v !second); end;
        partition_unwrapped ~n:next ~i:(i+1);
        if i < i_arg then begin
          let v = getv n in (first := insert !first v 0); end;
      end
  in let _ = partition_unwrapped ~n:ll.head ~i:0 in
  let ret = {head={v= !first; next={v= !second; next=ref None} |> Option.some |> ref} |> Option.some |> ref} in
    ret 