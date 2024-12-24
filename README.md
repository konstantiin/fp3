# Лабораторная работа №3. Лучинкин Константин
## Интерполяция 

## Код:
Main loop:
```ocaml
let rec main_loop points_last cnt config =
  let _ = Printer.print_msg "Input next point or command:" in
  match read_command () with
  | exception Invalid_argument s ->
      let _ = Printer.print_msg s in
      main_loop points_last cnt config
  | Exit -> ()
  | Freq fr ->
      let _ = Printer.print_msg "frequency set" in
      main_loop points_last cnt { frec = fr }
  | Point xy ->
      let points = xy :: points_last in
      let ifuncs = Interpolator.apply_data points in
      let _ =
        List.iter
          (fun (func : Interpolator.named_func) ->
            match func.f with
            | None -> Printer.print_not_enough func.name
            | Some f ->
                let lower_bound, _ = List.nth points (func.win_sz - 1) in
                let upper_bound, _ = xy in
                let pred = Utils.get_disc lower_bound upper_bound config.frec in
                let predicted = List.map f pred in
                Printer.print_points func.name
                  (List.rev (List.combine pred predicted)))
          ifuncs
      in
      main_loop points (cnt + 1) config
```
Interpolation methods:
```ocaml
let rec linear points x_pred =
  match points with
  | [] -> raise (Could_not_interpolate "not enough points")
  | (_, _) :: [] -> raise (Could_not_interpolate "not enough points")
  | (x0, y0) :: t -> (
      match t with
      | [] -> raise (Failure "case handled above")
      | (x1, y1) :: [] -> y0 +. ((y1 -. y0) /. (x1 -. x0) *. (x_pred -. x0))
      | (x1, y1) :: _ ->
          if x0 <= x_pred && x_pred <= x1 then
            y0 +. ((y1 -. y0) /. (x1 -. x0) *. (x_pred -. x0))
          else linear t x_pred)

let rec newton points x_pred =
  match points with
  | [] -> raise (Could_not_interpolate "not enough points")
  | (_, y0) :: [] -> y0
  | (xn, yn) :: t ->
      let g_prev = newton t in
      let ak =
        (yn -. g_prev xn)
        /. List.fold_left (fun prod (xi, _) -> prod *. (xn -. xi)) 1.0 t
      in
      let p =
        List.fold_left (fun prod (xi, _) -> prod *. (x_pred -. xi)) 1.0 t
      in
      (ak *. p) +. g_prev x_pred
```
Command parser:
```ocaml
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
```
## Пример
№1
```
Usage: setFreq <float>; exit; <float> <float>
Input next point or command:
0 0
For linear method I need more points
Results for method Newton:
  0.
  0.
Input next point or command:
1 1
Results for method linear:
  0.   1.
  0.   1.
Results for method Newton:
  0.   1.
  0.   1.
Input next point or command:
3 9
Results for method linear:
  1.   2.   3.
  1.   5.   9.
Results for method Newton:
  0.   1.   2.   3.
  0.   1.   4.   9.
```
№2
```bash
kostic@DESKTOP-4J5A7C7:~/fp/lab3$ dune exec bin/main.exe 0.5
Usage: exit; <float> <float>
Input next point or command:
1 2
For linear method I need more points
Results for method Newton:
  1.
  2.
Input next point or command:
3 4
Results for method linear:
  1.  1.5   2.  2.5   3.
  2.  2.5   3.  3.5   4.
Results for method Newton:
  1.  1.5   2.  2.5   3.
  2.  2.5   3.  3.5   4.
Input next point or command:
5 7
Results for method linear:
  3.  3.5   4.  4.5   5.
  4. 4.75  5.5 6.25   7.
Results for method Newton:
  1.  1.5   2.  2.5   3.  3.5   4.  4.5   5.
  2. 2.41 2.88 3.41   4. 4.66 5.38 6.16   7.
Input next point or command:
setFreq 1
frequency set
Input next point or command:
6 9
Results for method linear:
  5.   6.
  7.   9.
Results for method Newton:
  1.   2.   3.   4.   5.   6.
  2.  2.9   4. 5.35   7.   9.
Input next point or command:
exit
kostic@DESKTOP-4J5A7C7:~/fp/lab3$
```