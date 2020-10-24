open Graphics

(** [user_input] listens for user input. *)
let rec user_input () =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0
  else user_input ()

(** [setup_background color] changes the background color of the window to 
    [color]. *)
let setup_background color = 
  set_color color;
  fill_rect 0 0 (size_x ()) (size_y ());
  set_color foreground; ()

let main () =
  open_graph " 900x800";
  set_window_title "Go";
  setup_background (rgb 235 195 120);
  user_input ()

let () = main ()
