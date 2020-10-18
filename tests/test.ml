open Command
open Game
open OUnit2

(** [cmp_values] is an oUnit Test to determine whether [v1] equals [v2]. *)
let cmp_values name v1 v2 =
  name >:: (fun _ -> assert_equal v1 v2)

(** [load_file file] is the Go game representation of the contents in [file]. *)
let load_game file =
  Yojson.Basic.from_file file |> from_json

(* All the game files *)
let standard_19 = load_game "games/standard_19.json"
let game_one = load_game "games/game_one.json"

let command_tests = [

]

let game_tests = [
  cmp_values "standard board size is 19" 19 (board_size standard_19);
]

let go_tests = [
  
]

let suite =
  "GOcaml Test Suite"  >::: List.flatten [
    game_tests;
  ]

let _ = run_test_tt_main suite
