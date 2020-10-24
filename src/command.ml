
exception Deformed

exception Empty

exception KoException

type command = 
  | Pass
  | Play of string
  | Forfeit
  | Save of string
  | Quit

let istone_pos pos =
  let col = Char.code (String.get pos 0) - 65 in
  let row = int_of_string (Str.string_after pos 1) - 1
  in (col, row)

(** [filter_empty str_element] is the predicate used in List.filter to filter 
    out the empty strings *)
let filter_empty str_element = 
  String.length(str_element) > 0

(** [validate_phrase str_list] rasies [Empty] if [str_list] is empty and  
    Deformed if the str_list doesn't have a correct format. 
    Returns a command otherwise*)
let validate_phrase = function
  | [] -> raise Empty
  | h :: t -> begin
      if h = "play" && t != [] then Play (List.hd t)
      else if h = "save" && t != [] then Save (List.hd t)
      else if h = "pass" && t = [] then Pass
      else if h = "forfeit" && t = [] then Forfeit
      else if h = "quit" && t = [] then Quit
      else raise Deformed
    end

(** [too_long lst] returns [lst] if its length isn't greater than. Otherwise
    it raises Deformed. *)
let too_long lst = 
  if List.length lst > 2 then raise Deformed else lst

(** [check_size game pos] returns true if the position in within the board.
    Returns false otherwise. *)
let check_size game (col,row) =
  let board_size = Game.board_size game in
  col >= 0 && col < board_size 
  &&
  row >= 0 && row < board_size

(** [validate_play game cmd] makes sure that the stone position of the play 
    command is within the bounds of the board. *)
let validate_play game = function
  | Play str -> if check_size game (istone_pos str) then cmd else raise Deformed
  | _ -> cmd

let parse game str = 
  String.split_on_char ' ' str 
  |> List.map String.trim 
  |> List.filter filter_empty
  |> too_long 
  |> validate_phrase
  |> validate_play game 
