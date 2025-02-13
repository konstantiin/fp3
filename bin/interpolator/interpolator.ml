exception Could_not_interpolate of string

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

type named_func = { name : string; win_sz : int; f : (float -> float) option }

let apply_data data =
  let lin =
    match Utils.get_first_n 2 data with
    | None -> { name = "linear"; win_sz = 2; f = None }
    | Some d -> { name = "linear"; win_sz = 2; f = Some (linear d) }
  in
  let newt =
    { name = "newton"; win_sz = List.length data; f = Some (newton data) }
  in
  [ lin; newt ]
