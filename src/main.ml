open Command
open Game
open State

(** [create_game f] creates a game from a valid JSON file [f]. *)
let create_game f = 
  Yojson.Basic.from_file f |> from_json 

let print_new_turn () = 
  print_string "\n> "; ()

(** [play game] game while command isn't to quit. If command is play, update state. *)
let rec play game = 
  print_new_turn ();
  let user_input = read_line () in 
  try 
    match parse game user_input with
    | Quit -> exit 0
    | Pass -> failwith "unimplemented"
    | Forfeit -> failwith "unimplimented"
    | Save s -> failwith "unimplemented"
    | Play pos -> play (step game (istone_pos pos) 0)
  with 
    | Empty -> 
      print_endline "You didn't type anything! Try again!\n"; play game
    | Deformed -> 
      print_endline "That's not a valid command!\n"; play game
    | GoOutOfBounds ->
      print_endline "The position"; play game
    | StoneAlreadyExists ->
      print_endline "A stone already exists in that location."; play game

(** [main ()] prompts for the game to play, then starts it. *)
let main () =

  print_endline "Please enter the name of the game file you wish to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play (create_game file_name)

(* Execute the game engine. *)
let () = main ()

(*
  {| 


  |}
  print_endline "WELCOME TO GO !";
  print_endline "You currently have two commands:";
  print_endline "1. Type <play position> (excluding the gang signs) to place a stone at a position";
  print_endline "2. Type <quit> (excluding gang signs) to quit the game.";
  print_endline "Now that you know the basics, let's begin!\n";
  *)
