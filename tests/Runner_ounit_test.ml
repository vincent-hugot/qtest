let passing =
  QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list small_int)
    (fun l -> List.rev (List.rev l) = l);;

let failing =
  QCheck.(Test.make ~count:10 ~name:"failing_test"
            (list small_int)
            (fun l -> l = List.sort compare l));;

let () =
  let open QCheck_runner in
  let _ = OUnit.run_test_tt_main ("tests" >::: [passing; failing]) in
  ()
