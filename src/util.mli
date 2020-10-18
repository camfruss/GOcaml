
(** [combine op t1 t2] combines the elements of each tuple according to [op]. *)
val combine : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c

(** [string_of_int_list lst] concatenates all the ints in [lst] to a single 
    string. *)
val string_of_int_list : int list -> string
