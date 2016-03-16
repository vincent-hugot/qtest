let passing =
  QCheck.mk_test ~n:1000
    ~name:"list_rev_is_involutive"
    QCheck.Arbitrary.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  QCheck.(mk_test ~n:10
            ~pp:PP.(list int) ~name:"failing_test"
            QCheck.Arbitrary.(list small_int)
            (fun l -> l = List.sort compare l));;

let () =
  let open QCheck_runner in
  let _ = OUnit.run_test_tt_main ("tests" >::: [passing; failing]) in
  ()
