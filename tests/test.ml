open Command
open Game
open OUnit2

let game_tests = [

]

let suite =
  "GOcaml Test Suite"  >::: List.flatten [
    game_tests;
  ]

let _ = run_test_tt_main suite
