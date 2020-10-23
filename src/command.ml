
exception Deformed

exception Empty

exception KoException

type command = 
  | Pass
  | Play of string list
  | Forfeit
  | Save of string list
  | Quit

(** [istone_pos pos] is the integer position of [pos] TODO
    Requires: [pos] is a single capitalized alphabetic character followed by 
    integer in the interval [1,board_size]. *)
let istone_pos pos =
  let col = Char.code (String.get pos 0) - 65 in
  let row = int_of_string (Str.string_after pos 1) - 1
  in (col, row)

(** [filter_empty str_element] is the predicate used in List.filter to filter 
    out the empty strings *)
let filter_empty str_element = String.length(str_element) > 0

(** [validate_phrase str_list] rasies Empty if [str_list] is empty and  
    Deformed if the str_list doesn't have a correct format. 
    Returns a command otherwise*)
let validate_phrase str_list =
  match str_list with
  | [] -> raise Empty
  | h::t -> begin
      if h = "go" && t != [] then Play t
      else if h = "save" && t != [] then Save t
      else if h = "pass" && t = [] then Pass
      else if h = "forfeit" && t = [] then Forfeit
      else if h = "quit" && t = [] then Quit
      else raise Deformed
    end

let parse str = 
  String.split_on_char ' ' str 
  |> List.map String.trim 
  |> List.filter filter_empty 
  |> validate_phrase
