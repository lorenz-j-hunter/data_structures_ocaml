type 'a node = {v: 'a; next: ('a node option) ref}

type 'a sll = {head: ('a node option) ref}

val new_node : 'a -> 'a node

val new_sll : 'a -> 'a sll

val append : v:'a -> ll:'a sll -> unit

val size : ll:'a sll -> int

val search : ll:'a sll -> index:int -> 'a 

val insert : ll:'a sll -> v:'a -> index:int -> unit 

val remove : ll:'a sll -> index:int -> unit