open OUnit2
open Hash_bag

module HashableStr = struct
  type t = string

  let hash str =
    let s = String.to_seq str in
    Seq.fold_left (fun acc c -> acc + Char.code c) 0 s
end

module StringHBag = Make (HashableStr)

let str_bag = StringHBag.empty ()
let b1 = StringHBag.add "11" str_bag
let b2 = StringHBag.add "11" b1
let b3 = StringHBag.add "33" b2
let rb3 = StringHBag.remove "33" b3

let unit_tests =
  "unit tests"
  >::: [
         ( "add + find №1" >:: fun _ ->
           assert_equal true (StringHBag.find "11" b1) );
         ( "add + find №2" >:: fun _ ->
           assert_equal true (StringHBag.find "11" b1) );
         ( "add + find №3" >:: fun _ ->
           assert_equal false (StringHBag.find "22" b1) );
         ("count №1" >:: fun _ -> assert_equal 2 (StringHBag.count "11" b2));
         ("count №2" >:: fun _ -> assert_equal 2 (StringHBag.count "11" b3));
         ("count №3" >:: fun _ -> assert_equal 0 (StringHBag.count "22" b3));
         ("count №4" >:: fun _ -> assert_equal 1 (StringHBag.count "33" b3));
         ("remove №1" >:: fun _ -> assert_equal false (StringHBag.find "33" rb3));
         ("remove №2" >:: fun _ -> assert_equal 2 (StringHBag.count "11" rb3));
       ]

let _ = run_test_tt_main unit_tests

module IntHashBag = Make (struct
  type t = int

  let hash = Fun.id
end)

let add_remove_reversability =
  QCheck.Test.make ~count:10000 ~name:"add and remove opposite"
    QCheck.(pair (list (0 -- 1000)) (-1000 -- -1))
    (fun (lst, el) ->
      let bag = IntHashBag.(lst |> of_list |> add el) in
      IntHashBag.(equal (bag |> remove el) (of_list lst)))

let map_of_list_associativity =
  QCheck.Test.make ~count:10000
    ~name:"lst |> of_list |> map f <===> lst |> map f |> of_list"
    QCheck.(list (-1000 -- 1000))
    (fun lst ->
      let f = fun x -> x * 100 in
      let b1 = IntHashBag.map (module IntHashBag) (IntHashBag.of_list lst) f in
      IntHashBag.(equal (lst |> List.map f |> of_list) b1))

let monoid_asociativity =
  QCheck.Test.make ~count:10000 ~name:"monoid asociativity"
    QCheck.(
      triple
        (list (-1000 -- 1000))
        (list (-1000 -- 1000))
        (list (-1000 -- 1000)))
    (fun (lst1, lst2, lst3) ->
      let hm12 = IntHashBag.(of_list lst1 |> join (of_list lst2)) in
      let hm23 = IntHashBag.(of_list lst2 |> join (of_list lst3)) in
      IntHashBag.(
        equal (of_list lst1 |> join hm23) (hm12 |> join (of_list lst3))))

let monoid_neutral =
  QCheck.Test.make ~count:10000 ~name:"monoid neutral"
    QCheck.(list (-1000 -- 1000))
    (fun lst ->
      IntHashBag.(equal (of_list lst |> join (empty ())) (of_list lst)))

let _ =
  QCheck_runner.run_tests
    [
      add_remove_reversability;
      map_of_list_associativity;
      monoid_asociativity;
      monoid_neutral;
    ]

let _ = run_test_tt_main
