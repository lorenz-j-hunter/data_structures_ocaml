open Singly_linked_list.Sll

type 'a set = {elems: 'a sll}

let new_set elem = {elems: new_sll elem}

let rec to_list set =
  let rec unpack sll ret =
    match !(sll.head) with
    | None -> ()
    | Some cur -> 
      unpack {head: cur.next} ret;
      cur.v :: ret
  in unpack set.elems []

let member elem set =
  List.mem elem (to_list set) 

val not_member elem set =
  not (List.mem elem (to_list set))

val cardinality set =
  List.size (to_list set)

(*This only works for integer sets.*)
let rec is_ordered set ->
  match to_list set with
  | [] -> True
  | one :: [] -> True
  | fst :: snd :: tl ->
    if fst <= snd then
      let new_tail = remove set.elems ~index:0 in
        is_ordered new_tail
    else False

let union s1 s2 =
  let rec iter = fun (l: 'a sll) (acc: 'a sll) ->
    match !(l.head) with
    | None -> {elems: reverse acc}
    | Some node ->
      if List.exists node.v acc then
        iter {head: node.next} acc 
      else
        insert ~index:0 acc node.v;
        iter {head: node.next} acc 
  in iter (extend s1.elems s2.elems) []

let intersection s1 s2 =
  let rec iter = fun (l: 'a sll) (acc: 'a sll) ?(store: 'a list = []) ->
    match !(l.head) with
    | None -> {elems: acc} 
    | Some node ->
      if List.exists node.v store then
        append acc node.v
        iter {head: node.next} acc ~store
      else
        node.v :: store;
        iter {head: node.next} acc ~store 
  in iter (extend s1.elems s2.elems) []

(*Find the elements which are only in set 1.*)
let difference s1 s2 =
  let rec iter = fun (set1: 'a sll) (set2: 'a sll) ?(difference: 'a sll = new_sll (new_node 0)) ->
    match !(set1.head) with
    | None -> {elems: remove ~index: 0 difference}
    | Some node ->
      if member node set2 then
        iter (remove ~index:0 set1) set2 ~difference
      else
        iter (remove ~index:0 set1) set2 ~difference:(insert ~index:0 ~v:node difference)
  in iter s1.elems s2.elems

let sym_diff s1 s2 = 
  let rec iter = fun set1 set2 intersection ?(acc: 'a sll = new_sll (new_node 0)->
    match !(set1.head), !(set2.head) with
    | None, None -> {elems: acc}
    | Some node, None ->
      if member node intersection then
        (*don't put in*) 
      else
        (*put in*)
    | None, Some node ->
      if member node intersection then
        (*don't put in*) 
      else
        (*put in*)
    | Some node1, Some node2 ->
      if member node1 intersection then
        (*don't put in*) 
      else
        (*put in*)
      if member node2 intersection then
        (*don't put in*)
      else
        (*put in*)
      (*recursive call*)
  in iter s1.elems s2.elems (intersection s1 s2)

(*Check if s1 is a subset of s2.*)
let is_subset s1 s2 =
  if List.for_all (fun x -> member x s2) (to_list s1) then
    True
  else
    False

(*Check if s1 is a superset of s2.*)
let is_superset s1 s2 =
  if List.for_all (fun x -> member x s1) (to_list s2) then
    True
  else
    False

let is_disjoint s1 s2 =
  if List.for_all (fun x -> not_member x s2) (to_list s1) then
    True
  else
    False

val rbs : 'a -> 'a set -> bool

val rts : 'a -> 'a set -> bool (*recursive ternary search.*)