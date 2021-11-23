type t

val empty : t (*done*)

val is_empty : t -> bool (*done*)

val add : int * int -> t -> t

val remove : int * int -> t -> t

val mem : int -> t -> bool (*done*)

val iter : (int * int -> unit) -> t -> unit (*done*)?

val fold : (int * int -> 'a -> 'a) -> t -> 'a -> 'a (*done*)?

val elements : t -> (int * int) list (*done*)

val below : int -> t -> int  

val split : int -> t -> t * bool * t
