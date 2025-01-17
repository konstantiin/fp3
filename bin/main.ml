open Reader

type config = { freq : float }

let cfg =
  match Array.length Sys.argv with
  | 1 -> { freq = 1.0 }
  | 2 -> { freq = Sys.argv.(1) |> float_of_string }
  | _ ->
      let _ = Printer.print_msg "one argument supported" in
      exit 0

let rec main_loop points_last cnt config is_first =
  let _ = Printer.print_msg "Input next point or command:" in
  match read_command () with
  | exception Invalid_argument s ->
      let _ = Printer.print_msg s in
      main_loop points_last cnt config is_first
  | Exit -> ()
  | Freq fr ->
      let _ = Printer.print_msg "frequency set" in
      main_loop points_last cnt { freq = fr } is_first
  | Point xy ->
      let points = xy :: points_last in
      let ifuncs = Interpolator.apply_data points in
      let _ =
        List.iter
          (fun (func : Interpolator.named_func) ->
            match func.f with
            | None -> Printer.print_not_enough func.name
            | Some f ->
                let upper_bound, _ = xy in
                let lower_bound =
                  if is_first then upper_bound
                  else
                    let l, _ = List.nth points_last 0 in
                    l +. config.freq
                in
                let pred = Utils.get_disc lower_bound upper_bound config.freq in
                let predicted = List.map f pred in
                Printer.print_points func.name
                  (List.rev (List.combine pred predicted)))
          ifuncs
      in
      main_loop points (cnt + 1) config false

let _ = Printer.print_msg "Usage: setFreq <float>; exit; <float> <float>"
let _ = main_loop [] 0 cfg true
