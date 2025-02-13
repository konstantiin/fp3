open Reader

type config = { freq : float; algos : string list }

let usage_msg = " <algorithm 1> [<algorithm 2>] ... [-f] <frequency>"
let frequency = ref 1.0
let algorithms = ref []

let anon_fun algo =
  let _ =
    match algo with
    | "linear" -> ()
    | "newton" -> ()
    | _ ->
        raise
          (Failure (String.concat " " [ "argorithm"; algo; "not supported" ]))
  in
  algorithms := algo :: !algorithms

let speclist = [ ("-f", Arg.Set_float frequency, "Set frquency") ]
let () = Arg.parse speclist anon_fun usage_msg
let cfg = { freq = !frequency; algos = !algorithms }
let max_points = 5

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

let _ = Printer.print_msg "Usage: setFreq <float>; exit; <float> <float>"
let _ = main_loop [] 0 cfg [ ("newton", None); ("linear", None) ]
