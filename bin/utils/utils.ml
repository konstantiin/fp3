let rec get_first_n_acc acc n lst =
  if n = 0 then Some (List.rev acc)
  else
    match lst with [] -> None | h :: t -> get_first_n_acc (h :: acc) (n - 1) t

let get_first_n n lst = get_first_n_acc [] n lst

let rec get_disc_acc acc lb ub fr =
  if lb > ub then acc else get_disc_acc (lb :: acc) (lb +. fr) ub fr

let get_disc lb ub fr = get_disc_acc [] lb ub fr
