let print_not_enough meth =
  print_endline ("For " ^ meth ^ " method I need more points")

let print_points name points =
  let _ = print_endline ("Results for method " ^ name ^ ":") in
  let _ = List.iter (fun (x, _) -> Printf.printf "%4.3F %!" x) points in
  let _ = print_endline "" in
  let _ = List.iter (fun (_, y) -> Printf.printf "%4.3F %!" y) points in
  print_endline ""

let print_msg msg = print_endline msg
