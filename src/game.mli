
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

(** [board_size t] is the nxn dimension of the board in game [t]. *)
val board_size : t -> int

(** [stones t s] is the col and row of every stone on the board of type 
    [stone]. *)
val stones : t -> stone -> (int * int) list

(** [step t move time] is the new game after the current player places a stone 
    on [move] and spends [time] seconds doing so. *)
val step : t -> (int * int) -> int -> t
