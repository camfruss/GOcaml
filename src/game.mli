
(** [t] is the abstract type of values representing go games. *)
type t

(** [stone] is the type representing the black or white stones *)
type stone = Black | White

(** [from_json json] is the Go game that [json] represents.
    Requires: [json] is a valid json Go representation. *)
val from_json : Yojson.Basic.t -> t

(** [to_json go str] writes the contents of [go] into a valid json.
    Does not overwrite pre-existing files.. *)
val to_json : t -> string -> unit

(** [stones t s] is the col and row of every stone on the board of type 
    [stone]. *)
val stones : t -> stone -> (int * int) list

(** [board_size t] is the nxn dimension of the board in game [t]. *)
val board_size : t -> int

(** [get_p1_stone] returns the color stone of player 1 (either "b" or "w") *)
val get_p1_stone : t -> char

(** [get_p2_stone] returns the color stone of player 2 (either "b" or "w") *)
val get_p2_stone : t -> char
