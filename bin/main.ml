open Reader

type config = { frec : float }

let cfg =
  match Array.length Sys.argv with
  | 1 -> { frec = 1.0 }
  | 2 -> { frec = Sys.argv.(1) |> float_of_string }
  | _ ->
      let _ = Printer.print_msg "one argument supported" in
      exit 0

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

let _ = Printer.print_msg "Usage: exit; <float> <float>"
let _ = main_loop [] 0 cfg
