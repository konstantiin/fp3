# Лабораторная работа №3. Лучинкин Константин
## Интерполяция 

## Код:
Main loop:
```ocaml
let rec main_loop points_last cnt config last_printed =
  if List.length points_last = max_points + 1 then
    main_loop (Utils.drop_last points_last) cnt config last_printed;
  let _ = Printer.print_msg "Input next point or command:" in
  match read_command () with
  | exception Invalid_argument s ->
      let _ = Printer.print_msg s in
      main_loop points_last cnt config last_printed
  | Exit -> ()
  | Freq fr ->
      let _ = Printer.print_msg "frequency set" in
      main_loop points_last cnt { freq = fr; algos = config.algos } last_printed
  | Point xy ->
      let points = xy :: points_last in
      let ifuncs = Interpolator.apply_data points in
      let new_last_printed =
        List.map
          (fun (func : Interpolator.named_func) ->
            let is_allowed =
              List.find_opt (fun allowed -> allowed = func.name) config.algos
            in
            let upper_bound, _ = xy in
            let lower_bound =
              let last_cur = List.assoc func.name last_printed in
              match last_cur with None -> upper_bound | Some l -> l
            in
            let pred = Utils.get_disc lower_bound upper_bound config.freq in
            if is_allowed = None then (func.name, None)
            else if pred = [] then (func.name, Some lower_bound)
            else
              match func.f with
              | None ->
                  Printer.print_not_enough func.name;
                  (func.name, Some lower_bound)
              | Some f ->
                  let predicted = List.map f pred in
                  Printer.print_points func.name
                    (List.rev (List.combine pred predicted));
                  (func.name, Some (List.hd pred +. config.freq)))
          ifuncs
      in
      main_loop points (cnt + 1) config new_last_printed
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
$ dune exec bin/main.exe newton linear
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
  1.
  1.
Results for method Newton:
  1.
  1.
Input next point or command:
3 9
Results for method linear:
  2.   3.
  5.   9.
Results for method Newton:
  2.   3.
  4.   9.
```
№2
```
~/fp/lab3$ ./_build/default/bin/main.exe -f 0.5 linear
Usage: setFreq <float>; exit; <float> <float>
Input next point or command:
1 1
For linear method I need more points
Input next point or command:
2 2
Results for method linear:
  1.  1.5   2.
  1.  1.5   2.
Input next point or command:
3 3
Results for method linear:
 2.5   3.
 2.5   3.
Input next point or command:
exit
~/fp/lab3$
```
№3

```
~/fp/fp3$ ./_build/default/bin/main.exe -f 0.7 linear
Usage: setFreq <float>; exit; <float> <float>
Input next point or command:
1 1
For linear method I need more points
Input next point or command:
2 2
Results for method linear:
  1.  1.7  3.1
  1.  1.7  3.1
Input next point or command:
4 4
Results for method linear:
 3.8  5.2
 3.8  5.2
Input next point or command:
5 5
Input next point or command:
6 6
Results for method linear:
 5.9  7.3
 5.9  7.3
Input next point or command:
exit
~/fp/fp3$
```