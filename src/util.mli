
(** [combine op t1 t2] combines the elements of each tuple according to [op]. *)
val combine : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c

(** [string_of_string_list sl] concatenates all the strings in [sl] to a single 
    string. *)
val string_of_string_list : string list -> string
