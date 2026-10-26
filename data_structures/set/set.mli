(*Will not expose the real set.*)
open Singly_linked_list.Sll

type set = {elems: int sll}

val new_set : int -> set

val to_list : set -> int list

val member : int -> set -> bool (*O(n) time, will search on list instead of rbs.*)

val not_member : int -> set -> bool

val cardinality : set -> int

val is_ordered : set -> bool

val union : set -> set -> set

val intersection : set -> set -> set

val difference : set -> set -> set

val sym_diff : set -> set -> set

val is_subset : set -> set -> bool

val is_superset : set -> set -> bool

val is_disjoint : set -> set -> bool

(*
val rbs : int -> set -> bool

val rts : int -> set -> bool (*recursive ternary search.*)
*)