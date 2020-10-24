open Command
open Game
open State

(** [create_game f] creates a game from a valid JSON file [f]. *)
let create_game f = 
  Yojson.Basic.from_file f |> from_json 

(** [playing] game while command isn't to quit. If command is play, update state. *)
let rec playing game st  = 
  st |> State.turn_start_text game

  print_endline "\n";
  print_string "> ";

  let user_input = read_line() in 

  try 
    match Command.parse game user_input with
    | Quit -> exit 0
    | Pass -> failwith "unimplemented"
    | Forfeit -> failwith "unimplimented"
    | Save str1 -> failwith "unimplemented"
    | Play str2 -> let move_tuple = Command.istone_pos str2 in 
      match State.play move_tuple game st with 
      | Illegal -> 
        print_endline "There is already a stone in that position. Please try somewhere else.\n";
        playing game st ;
      | Legal new_st -> playing game new_st 

  with 
  | Empty -> print_endline "You didn't type anything! Try again!\n";
    playing game st;
  | Deformed -> print_endline "That's not a valid command!\n";
    playing game st; 
;;

let play_game f =
  let game = create_game f in 
  playing game (init_state game)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =

  print_endline "Please enter the name of the game file you wish to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()


  {| 
  |}
  print_endline "WELCOME TO GO !";
  print_endline "You currently have two commands:";
  print_endline "1. Type <play position> (excluding the gang signs) to place a stone at a position";
  print_endline "2. Type <quit> (excluding gang signs) to quit the game.";
  print_endline "Now that you know the basics, let's begin!\n";
