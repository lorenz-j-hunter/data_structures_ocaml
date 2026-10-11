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

let member elem set =
  exists elem set.elems

let not_member elem set =
  not (exists elem set.elems)

let cardinality set =
  size set.elems

(*This only works for integer sets.*)
let rec is_ordered = fun set ->
  let rec is_ordered_unwrapped = fun (cur: 'a node option ref) (prev: 'a node option ref) ->
    match !node with
    | None -> true
    | Some unpeeled_node -> 
      if (unpeel prev).v <= unpeeled_node.v then
        is_ordered_unwrapped (cur=cur.next) (prev=prev.next)
      else false
  in
    if size set.elems > 2 then
      is_ordered_unwrapped (cur=ll.head|>unpeel|>getnext) (prev=ll.head)
    else if size set.elems == 2 then
      let cur = ll.head |> unpeel |> getnext |> unpeel
      and prev = ll.head |> unpeel in
        if prev.v <= cur.v then true
        else false
    else true

let union s1 s2 =
  let rec iter = fun (l: 'a sll) (acc: 'a sll) ->
    match !(l.head) with
    | None -> reverse acc; {elems=acc}
    | Some node ->
      if exists node.v acc then
        iter {head=node.next} acc 
      else
        insert 0 (ll=acc) (v=node.v);
        iter {head=node.next} acc 
  in iter (extend s1.elems s2.elems) []

let intersection s1 s2 =
  let rec iter = fun (l: 'a sll) (acc: 'a sll) ?(store: 'a list = []) ->
    match !(l.head) with
    | None -> {elems=acc} 
    | Some node ->
      if List.exists node.v store then
        append acc node.v
        iter {head=node.next} acc ~store
      else
        node.v :: store;
        iter {head=node.next} acc ~store 
  in iter (extend s1.elems s2.elems) []

(*Find the elements which are only in set 1.*)
let difference s1 s2 =
  let rec iter = fun (set1: 'a sll) (set2: 'a sll) ?(difference: 'a sll = new_sll (new_node 0)) ->
    match !(set1.head) with
    | None -> {elems=(remove ~index: 0 difference)}
    | Some node ->
      if member node set2 then
        iter (remove 0 set1) set2 ~difference
      else
        iter (remove 0 set1) set2 ~difference:(insert (index=0) (v=node) difference)
  in iter s1.elems s2.elems

let sym_diff s1 s2 = 
  let rec iter = fun set1 set2 intersection ?(acc: 'a sll = new_sll (new_node 0)) ->
    match !(set1.head), !(set2.head) with
    | None, None -> {elems=acc}
    | Some node, None ->
      if member node intersection then
        (*don't put in*)
        iter (set1={head=node.next}) set2 ~acc
      else
        (*put in*)
        iter (set1={head=node.next}) set2 ~acc:(insert (index=0) (ll=acc) (v=node.v))
    | None, Some node ->
      if member node intersection then
        (*don't put in*) 
        iter set1 (set2={head=node.next}) ~acc
      else
        (*put in*)
        iter set1 (set2={head=node.next}) ~acc:(insert 0 (ll=acc) (v=node.v))
    | Some node1, Some node2 ->
      if member node1 intersection & member node2 intersection then
        (*if both are members of intersection, add neither.*)
        iter (set1={head=node1.next}) (set2={head=node1.next}) ~acc
      else if member node1 intersection & not (member node2 intersection) then
        (*if node2 is a member of the intersection, add it.*)
        let new_acc = insert ~index:0 ~ll:acc ~v:node2.v in
          iter (set1={head=node1.next}) (set2={head=node1.next}) ~acc:new_acc
      else if not (member node1 intersection) & member node2 intersection then
        (*if node1 is a member of the intersection, add it.*)
        let new_acc = insert 0 (ll=acc) (v=node1.v) in
          iter (set1={head=node1.next}) (set2={head=node1.next}) ~acc:new_acc
      else if not (member node1 intersection) & not (member node2 intersection) then
        (*if both are members  of the intersection, add them both.*)
        let new_acc = insert 0 (ll=acc) (v=node1.v) in
        let new_new_acc = insert 0 (ll=acc) (v=node2.v) in
          iter (set1={head=node1.next}) (set2={head=node1.next}) ~acc:new_new_acc
  in iter s1.elems s2.elems (intersection s1 s2)

(*Check if s1 is a subset of s2.*)
let is_subset s1 s2 =
  if List.for_all (fun x -> member x s2) (to_list s1) then
    true
  else
    false

(*Check if s1 is a superset of s2.*)
let is_superset s1 s2 =
  if List.for_all (fun x -> member x s1) (to_list s2) then
    true
  else
    false

let is_disjoint s1 s2 =
  if List.for_all (fun x -> not_member x s2) (to_list s1) then
    true
  else
    false

(*
let rbs : 'a -> 'a set -> bool

let rts : 'a -> 'a set -> bool (*recursive ternary search.*)
*)