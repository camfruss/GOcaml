
(** [combine op t1 t2] combines the elements of each tuple according to [op]. *)
val combine : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c

(** TODO *)
val string_of_string_list : string list -> string