open OUnit2

let unit_tests =
  "unit tests"
  >::: [
         ( "utils №1" >:: fun _ ->
           assert_equal [ 3.0; 2.0; 1.0 ] (Utils.get_disc 1.0 3.0 1.0) );
         ("utils №2" >:: fun _ -> assert_equal None (Utils.get_first_n 2 [ 1 ]));
         ( "utils №3" >:: fun _ ->
           assert_equal
             (Some [ 1.0; 2.0 ])
             (Utils.get_first_n 2 [ 1.0; 2.0; 3.0 ]) );
         ( "interpolator №1" >:: fun _ ->
           assert_equal 2.0
             (match Interpolator.apply_data [ (1.0, 2.0) ] with
             | [] -> -1.0
             | _ :: [] -> -1.0
             | [ _; f2 ] -> (
                 match f2.f with None -> -1.0 | Some func -> func 1234.0)
             | _ :: _ :: _ -> -1.0) );
         ( "interpolator №2" >:: fun _ ->
           assert_equal "ok"
             (match Interpolator.apply_data [ (1.0, 2.0) ] with
             | [] -> "not ok"
             | _ :: [] -> "not ok"
             | [ f1; _ ] -> (
                 match f1.f with None -> "ok" | Some _ -> "not ok")
             | _ :: _ :: _ -> "not ok") );
         ( "interpolator №3" >:: fun _ ->
           assert_equal 4.0
             (match
                Interpolator.apply_data [ (3.0, 9.0); (1.0, 1.0); (0.0, 0.0) ]
              with
             | [] -> -1.0
             | _ :: [] -> -1.0
             | [ _; f2 ] -> (
                 match f2.f with None -> -1.0 | Some func -> func 2.0)
             | _ :: _ :: _ -> -1.0) );
         ( "interpolator №4" >:: fun _ ->
           assert_equal 5.0
             (match
                Interpolator.apply_data [ (3.0, 9.0); (1.0, 1.0); (0.0, 0.0) ]
              with
             | [] -> -1.0
             | _ :: [] -> -1.0
             | [ f1; _ ] -> (
                 match f1.f with None -> -1.0 | Some func -> func 2.0)
             | _ :: _ :: _ -> -1.0) );
       ]

let _ = run_test_tt_main unit_tests
