type 'a node = {v: 'a; next: 'a node option}

type 'a sll = {head: 'a node option}

val new_sll : 'a -> 'a sll

val append : v:'a -> ll:'a sll -> unit

val size : ll:'a sll -> int

(*val search : int -> 'a sll -> 'a *)
