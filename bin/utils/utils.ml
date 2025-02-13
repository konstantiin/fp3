let rec get_first_n_acc acc n lst =
  if n = 0 then Some (List.rev acc)
  else
    match lst with [] -> None | h :: t -> get_first_n_acc (h :: acc) (n - 1) t

let get_first_n n lst = get_first_n_acc [] n lst

let rec get_disc_acc acc lb ub fr =
  try
    if lb > ub then if List.hd acc < ub then (lb +. fr) :: acc else acc
    else get_disc_acc (lb :: acc) (lb +. fr) ub fr
  with Failure _ -> []

let get_disc lb ub fr = get_disc_acc [] lb ub fr

let rec drop_last list =
  match list with [] -> [] | _ :: [] -> [] | h :: t -> h :: drop_last t
