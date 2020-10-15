
exception Deformed

exception Empty

exception KoException

type command = 
  | Pass
  | Play of string * int
  | Forfeit
  | Save of string

let parse s = 
  failwith "unimplemented"
  