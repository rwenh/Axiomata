(** Pure rendering: model -> Notty image. *)

open Notty
open Notty.Infix

let attr_prompt = A.(fg lightblue)
let attr_input = A.(fg white)
let attr_cursor = A.(bg lightwhite ++ fg black)
let attr_output = A.(fg lightgreen)
let attr_error = A.(fg lightred)
let attr_info = A.(fg lightblack)
let attr_status = A.(fg yellow)

let render_entry = function
  | Model.Input s -> I.string attr_prompt "> " <|> I.string attr_input s
  | Model.Output s -> I.string attr_output ("  " ^ s)
  | Model.Error s -> I.string attr_error ("  ! " ^ s)
  | Model.Info s -> I.string attr_info ("  " ^ s)

let render_input ~(cols : int) (m : Model.t) : image =
  let prompt = I.string attr_prompt "> " in
  let before = String.sub m.input 0 m.cursor in
  let cursor_char =
    if m.cursor < String.length m.input then
      String.make 1 m.input.[m.cursor]
    else " "
  in
  let after_rest =
    if m.cursor + 1 < String.length m.input then
      String.sub m.input (m.cursor + 1) (String.length m.input - m.cursor - 1)
    else ""
  in
  let line =
    prompt
    <|> I.string attr_input before
    <|> I.string attr_cursor cursor_char
    <|> I.string attr_input after_rest
  in
  (* Truncate/pad to terminal width roughly *)
  let w = I.width line in
  if w > cols then I.hcrop (w - cols) 0 line else line

let render ~(rows : int) (m : Model.t) : image =
  let cols, _ = 80, rows in
  (* history fills everything above the input line and a status line *)
  let hist_imgs = List.map render_entry m.history in
  let hist =
    match hist_imgs with
    | [] -> I.empty
    | _ -> List.fold_left (fun acc i -> acc <-> i) (List.hd hist_imgs) (List.tl hist_imgs)
  in
  let input_line = render_input ~cols m in
  let status =
    I.string attr_status
      (Printf.sprintf "  vars: %d  |  :help  :q" (List.length (Cas.Env.to_list m.env)))
  in
  let body = hist <-> I.void 1 1 <-> input_line <-> status in
  (* Keep the bottom of the history visible if it overflows *)
  let h = I.height body in
  if h > rows then I.vcrop (h - rows) 0 body else body
