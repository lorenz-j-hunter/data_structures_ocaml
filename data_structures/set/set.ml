open Singly_linked_list.Sll

type 'a set = {elems: 'a sll}

let new_set elem = {elems=(new_sll elem)}

let rec to_list set =
  let rec unpack sll ret =
    match !(sll.head) with
    | None -> ret 
    | Some cur -> 
      unpack {head=cur.next} (ret @ [cur.v]);
  in unpack set.elems []

let member (elem: 'a) (set: 'a set) =
  exists elem set.elems

let not_member (elem: 'a) (set: 'a set) =
  not (exists elem set.elems)

let cardinality set =
  size set.elems

(*This only works for integer sets.*)
let rec is_ordered = fun set ->
  let rec is_ordered_unwrapped = fun ~(cur: 'a node option ref) ~(prev: 'a node option ref) ->
    match !cur with
    | None -> true
    | Some unpeeled_node -> 
      try
        if (unpeel prev).v <= unpeeled_node.v then
          is_ordered_unwrapped ~cur:(unpeeled_node.next) ~prev:((unpeel prev).next)
        else false
      with exn -> failwith "is_ordered: invalid argument";
  in
    if size set.elems > 2 then
      is_ordered_unwrapped ~cur:(set.elems.head|>unpeel|>getnext) ~prev:set.elems.head
    else if size set.elems == 2 then
      let cur = set.elems.head |> unpeel |> getnext |> unpeel
      and prev = set.elems.head |> unpeel in
        if prev.v <= cur.v then true
        else false
    else true

(*Order is preserved. If s1 and s2 are ordered sets, union s1 s2 will be an ordered set.*)
let union s1 s2 =
  let rec iter = fun (l: 'a sll) (acc: 'a sll) ->
    let next = l.head |> unpeel |> getnext in
    match !next with
    | None ->
      let node = l.head |> unpeel in
        if exists node.v acc then begin
          let ret = acc |> reverse in 
            {elems=ret} end
        else begin
          let ret = insert acc (getv l.head) 0 |> reverse in (
            {elems=ret} ) end
    | Some _ ->
      let node = l.head |> unpeel in
        if exists node.v acc then begin
          iter {head=node.next} acc end
        else begin
          iter {head=node.next} (insert acc node.v 0) end
  in 
    if size s1.elems = 0 && size s2.elems = 0 then
      {elems={head=ref None}}
    else
      iter (extend s1.elems s2.elems) {head=(ref None)}

(*Order is preserved. If s1 and s2 are ordered sets, intersection s1 s2 will be an ordered set.*)
let intersection (s1: 'a set) (s2: 'a set) =
  let rec iter = fun ?(store: 'a sll = {head=ref None}) (l: 'a sll) (acc: 'a sll)  ->
    match !(l.head) with
    | None -> {elems=reverse acc} 
    | Some node ->
      if exists node.v store then
        iter ~store {head=node.next} (insert acc node.v 0)
      else
        iter ~store:(insert store node.v 0) {head=node.next} acc  
  in iter (extend s1.elems s2.elems) {head=(ref None)} 

(*Find the elements which are only in set 1.*)
let difference (s1: 'a set) (s2: 'a set) =
  let rec iter = fun (e1: 'a sll) (acc: 'a sll) ->
    match !(e1.head) with
    | None -> {elems=reverse acc}
    | Some node ->
      if member node.v s2 then
        iter {head=node.next} acc
      else
        iter {head=node.next} (insert acc node.v 0)
  in iter s1.elems {head=ref None}

(*Order is preserved.*)
let sym_diff (s1: 'a set) (s2: 'a set) =
  (*Determine which elements are not in the intersection.*)
  let d1 = difference s1 s2 and d2 = difference s2 s1 in
    {elems=extend d1.elems d2.elems}

(*Check if s1 is a subset of s2.*)
let is_subset s1 s2 =
  let rec iter = fun (e1: 'a sll) ->
    match !(e1.head) with
    | None -> true 
    | Some node ->
      if member node.v s2 then
        iter {head=node.next}
      else
        false
  in iter s1.elems

(*Check if s1 is a superset of s2.*)
let is_superset s1 s2 =
  is_subset s2 s1

let is_disjoint s1 s2 =
  let rec iter = fun (e1: 'a sll) ->
    match !(e1.head) with
    | None -> true 
    | Some node ->
      if not_member node.v s2 then
        iter {head=node.next}
      else
        false
  in iter s1.elems

let rbs v (s: int set) = 
  let rec iter = fun (the_set: int set) (v: int) ->
    try
      let partitioned = partition the_set.elems ( (size the_set.elems) / 2 ) in
      let middle = search (search partitioned 1) 0 and lhs = search partitioned 0 and rhs = search partitioned 1 in
        match (v < middle), (v > middle), (v = middle) with
        | true, false, false -> iter {elems=lhs} v
        | false, true, false -> iter {elems=rhs} v
        | false, false, true -> true
        | _ -> if not_member v the_set then false else true
    with exn -> if Printexc.to_string exn = "Failure(\"partition: invalid argument\")" then false else raise exn
  in iter s v
