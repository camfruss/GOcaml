
(** [combine_t op t1 t2] combines the elements of each tuple according to 
    [op]. *)
val combine_t : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c

(** [string_of_int_list lst] concatenates all the ints in [lst] to a single 
    string. *)
val string_of_int_list : int list -> string

(** [max_triple3 lst] finds the largest third element of a triple in [lst]. *)
val max_triple3 : (int * int * int) list -> int
