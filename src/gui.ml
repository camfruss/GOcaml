open Game
open Graphics
open Util

(* let spacing = ref 0 *)

let turn = ref false
let toggle_color () = 
  turn := not !turn;
  set_color (if !turn then white else black);

let nearest_multiple a x b s = 
  let x = x - 2 * s in
  let estimate = (Float.ceil ((float_of_int x) /. (float_of_int s)) |> int_of_float) * s in
  estimate
  (* if estimate < a then a 
  else if estimate > b then b 
  else estimate  *)

(** [user_input] listens for user input. *)
let rec user_input () =
  let event = wait_next_event [Key_pressed; Button_down] in
  match event with
  | {mouse_x; mouse_y; button; keypressed; key} ->
    if keypressed then 
      if event.key == 'q' then exit 0
      else user_input ()
    else if button then
      let actual_x = nearest_multiple 0 mouse_x (grid_size * 1) 1 in
      let actual_y = nearest_multiple 0 mouse_y (grid_size * 1) 1 in
      fill_circle actual_x actual_y 10;
      toggle_color (); user_input ()

(** [setup_background color] changes the background color of the window to 
    [color]. *)
let setup_background color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color foreground; ()

let setup_grid game length s = 
  let lines = Array.make (bounds game) (0, 0, 0, 0) in
  for i = 0 to Array.length lines - 1 do
    lines.(i) <- ((2 + i) * s, 2 * s, (2 + i) * s, length - 2 * s)
  done;
  let segments = Array.append lines (quartet_swap lines) in 
  draw_segments segments; ()

let constraints game = 
  let grid_size = bounds game in
  let spacing = 
    800.0 /. (3.0 +. float_of_int grid_size) 
    |> Float.ceil 
    |> int_of_float in
  let length = spacing * (grid_size + 3) in
  (length, spacing)

let setup_axis game length s = 
  set_font "-*-Helvetica-medium-r-normal--18-*-*-*-*-*-iso8859-1";
  let grid_size = (bounds game) in
  let label_axis axis = 
    for j = 0 to 1 do
      let offset = if j = 0 then 0 else length in
      for i = 0 to grid_size - 1 do
        let loc = ((2 + i) * s), (offset - s |> Int.abs) in
        let s = match axis, loc with
        | `X, (x, y) ->
          let y_offset = if j = 1 then 22 else 0 in
          moveto (x - 5) (y - y_offset); 
          Char.escaped (Char.chr (65 + i))
        | `Y, (x, y) -> 
          let x_offset = if j = 1 then 10 else 0 in
          moveto (y - x_offset) (x - 9); 
          string_of_int (grid_size - i)
        in draw_string s;
      done
    done; ()
  in label_axis `X; label_axis `Y; ()

let setup_reference_pts game length s =
   failwith "unimplmented"

let main () =
  let game = Yojson.Basic.from_file "games/standard_19.json" |> from_json in (* TODO: remove *)
  let length, spacing = constraints game in
  open_graph (Printf.sprintf " %dx%d" length length);
  set_window_title "Go";
  setup_background (rgb 235 195 120);
  setup_grid game length spacing;
  setup_axis game length spacing;
  user_input ()

let () = main ()
