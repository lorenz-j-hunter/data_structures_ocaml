open Singly_linked_list.Sll

type 'a set = {elems: 'a sll}

val new_set : 'a -> 'a set

val to_list : 'a set -> 'a list

val member : 'a -> 'a set -> bool 

val not_member : 'a -> 'a set -> bool

val cardinality : 'a set -> int

val is_ordered : 'a set -> bool

val union : 'a set -> 'a set -> 'a set

val intersection : 'a set -> 'a set -> 'a set

val difference : 'a set -> 'a set -> 'a set

val sym_diff : 'a set -> 'a set -> 'a set

val is_subset : 'a set -> 'a set -> bool

val is_superset : 'a set -> 'a set -> bool

val is_disjoint : 'a set -> 'a set -> bool

(*
val rbs : int -> 'a set -> bool

val rts : int -> 'a set -> bool (*recursive ternary search.*)
*)