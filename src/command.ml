
exception Deformed

exception Empty

exception KoException

type command = 
  | Pass
  | Play of string
  | Forfeit
  | Save of string

(** [istone_pos pos] is the integer position of [pos] TODO
    Requires: [str] is a single capitalized alphabetic character followed by 
    integer in the interval [0,board_size). *)
let istone_pos pos =
  let col = Char.code (String.get pos 0) - 65 in
  let row = int_of_string (Str.string_after pos 1) 
  in (col, row)

let parse s = 
  failwith "unimplemented"
  