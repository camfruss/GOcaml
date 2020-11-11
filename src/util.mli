
(** [combine_t op t1 t2] combines the elements of each tuple according to 
    [op]. *)
val combine_t : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c

(** [string_of_list cast lst] concatenates all the elements in [lst] to a single 
    string, with each element cast to a string according to [cast]. *)
val string_of_list : ('a -> string) -> 'a list -> string

(** [max_triple3 lst] finds the largest positive third element of a triple in 
    [lst]. If [lst] is empty, returns 0. *)
val max_triple3 : (int * int * int) list -> int

(** [quartet_swap a] swaps the 1st with the 2nd and the 3rd with the 4th element
    in each quartet. *)
val quartet_swap : ('a * 'b * 'c * 'd) array -> ('b * 'a * 'd * 'c) array
