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
      if (unpeel prev).v <= unpeeled_node.v then
        is_ordered_unwrapped ~cur:(unpeeled_node.next) ~prev:((unpeel prev).next)
      else false
  in
    if size set.elems > 2 then
      is_ordered_unwrapped ~cur:(set.elems.head|>unpeel|>getnext) ~prev:set.elems.head
    else if size set.elems == 2 then
      let cur = set.elems.head |> unpeel |> getnext |> unpeel
      and prev = set.elems.head |> unpeel in
        if prev.v <= cur.v then true
        else false
    else true

let union s1 s2 =
  let rec iter = fun (l: 'a sll) (acc: 'a sll) ->
    match !(l.head) with
    | None -> {elems=(reverse acc)}
    | Some node ->
      if exists node.v acc then
        iter {head=node.next} acc 
      else
        iter {head=node.next} (insert acc node.v 0)
  in iter (extend s1.elems s2.elems) {head=(ref None)}

let intersection s1 s2 =
  let rec iter = fun ?(store: 'a list = []) (l: 'a sll) (acc: 'a sll)  ->
    match !(l.head) with
    | None -> {elems=acc} 
    | Some node ->
      if List.mem node.v store then
        iter ~store {head=node.next} (append node.v acc)
      else
        iter ~store:(node.v :: store) {head=node.next} acc  
  in iter (extend s1.elems s2.elems) {head=(ref None)} 

(*Find the elements which are only in set 1.*)
let difference s1 s2 =
  let rec iter = fun ?(difference: 'a sll = {head=(ref None)}) (elems1: 'a sll) ->
    match !(elems1.head) with
    | None -> {elems=(remove difference 0)}
    | Some node ->
      if member node.v s2 then
        iter ~difference (remove elems1 0) 
      else
        iter ~difference:(insert difference node.v 0) (remove elems1 0) 
  in iter s1.elems

let sym_diff (s1: 'a set) (s2: 'a set) = 
  let intersection = intersection s1 s2 in
  let rec iter = fun ?(acc: 'a sll = {head=(ref None)}) (elems1: 'a sll) (elems2: 'a sll) ->
    match !(elems1.head), !(elems2.head) with
    | None, None -> {elems=acc}
    | Some node, None ->
      if member node.v intersection then
        (*don't put in*)
        iter ~acc (*elems1*){head=node.next} elems2
      else
        (*put in*)
        iter ~acc:(insert acc node.v 0) (*elems1*){head=node.next} elems2
    | None, Some node ->
      if member node.v intersection then
        (*don't put in*) 
        iter ~acc elems1 (*elems2*){head=node.next}
      else
        (*put in*)
        iter ~acc:(insert acc node.v 0) elems1 (*elems2*){head=node.next}
    | Some node1, Some node2 ->
      if member node1.v intersection && member node2.v intersection then
        (*if both are members of intersection, add neither.*)
        iter ~acc (*elems1*){head=node1.next} (*elems2*){head=node2.next}
      else if member node1.v intersection && not (member node2.v intersection) then
        (*if node2 is a member of the intersection, add it.*)
        let new_acc = insert acc node2.v 0 in
          iter ~acc:new_acc (*elems1*){head=node1.next} (*elems2*){head=node2.next} 
      else if not (member node1.v intersection) && member node2.v intersection then 
        (*if node1 is a member of the intersection, add it.*)
        let new_acc = insert acc node1.v 0 in
          iter ~acc:new_acc (*elems1*){head=node1.next} (*elems2*){head=node2.next} 
      else 
        (*if both are members  of the intersection, add them both.*)
        let new_acc = insert acc node1.v 0 in
        let new_new_acc = insert new_acc node2.v 0 in
          iter ~acc:new_new_acc (*elems1*){head=node1.next} (*elems2*){head=node2.next}  
  in iter s1.elems s2.elems 

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