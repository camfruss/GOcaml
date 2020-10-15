
(** TODO, meaning of each field. *)
type play = {
  col : string;
  row : int;
  move : int; 
}

type t = {
  size : int;
  white : play list;
  black : play list
}
