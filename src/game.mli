
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
