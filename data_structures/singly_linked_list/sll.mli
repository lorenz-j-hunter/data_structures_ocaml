type 'a node = {v: 'a; next: ('a node option) ref}

type 'a sll = {head: ('a node option) ref}

val unpeel : ('a node option) ref -> 'a node

val getnext : 'a node -> ('a node option) ref

val new_node : 'a -> 'a node

val new_sll : 'a -> 'a sll

val append : 'a -> 'a sll -> 'a sll

val size : 'a sll -> int

val search : 'a sll -> int -> 'a

val exists : 'a -> 'a sll -> bool

val insert : 'a sll -> 'a -> int -> 'a sll 

val remove : 'a sll -> int -> 'a sll 

val extend : 'a sll -> 'a sll -> 'a sll 

val reverse : 'a sll -> 'a sll 

val partition : 'a sll -> int -> 'a sll list