
(** [from_json json] is the Go game that [json] represents.
    Requires: [json] is a valid json Go representation. *)
val from_json : Yojson.Basic.t -> Game.t

(** [to_json go str] writes the contents of [go] into a valid json.Authors
    Raises: [TODO] if a file already exists. *)
val to_json : Go.t -> string -> unit
