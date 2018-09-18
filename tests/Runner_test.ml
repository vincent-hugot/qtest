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
  let _ = QCheck_runner.run_tests_main [passing; failing] in
  ()
