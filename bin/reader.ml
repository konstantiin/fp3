type command = Exit | Point of (float * float) | Freq of float

let raise_inv_inp_ex () = invalid_arg "must be point (x y), or exit command"

let read_command =
 fun () ->
  match read_line () with
  | "" -> raise_inv_inp_ex ()
  | points -> (
      match Str.(split (regexp "[ \t]+") points) with
      | [] -> raise_inv_inp_ex ()
      | "exit" :: [] -> Exit
      | _ :: [] -> raise_inv_inp_ex ()
      | [ "setFreq"; arg ] -> Freq (float_of_string arg)
      | [ x_str; y_str ] -> Point (float_of_string x_str, float_of_string y_str)
      | _ :: _ :: _ -> raise_inv_inp_ex ())
