open OUnit

let ps,pl = print_string,print_endline
let va = Printf.sprintf
let pf = Printf.printf

let separator1 = String.make 79 '\\'
let separator2 = String.make 79 '/'

let string_of_path path =
  let path = List.filter (function Label _ -> true | _ -> false) path in
  String.concat ">" (List.rev_map string_of_node path)

let result_path = function
    | RSuccess path
    | RError (path, _)
    | RFailure (path, _)
    | RSkip (path, _)
    | RTodo (path, _) -> path

let result_msg = function
    | RSuccess _ -> "Success"
    | RError (_, msg)
    | RFailure (_, msg)
    | RSkip (_, msg)
    | RTodo (_, msg) -> msg

let result_flavour = function
    | RError _ -> "Error"
    | RFailure _ -> "Failure"
    | RSuccess _ -> "Success"
    | RSkip _ -> "Skip"
    | RTodo _ -> "Todo"

let not_success = function RSuccess _ -> false | _ -> true

let print_result_list =
  List.iter (fun result -> pf "%s\n%s: %s\n\n%s\n%s\n"
    separator1 (result_flavour result)
    (string_of_path (result_path result))
    (result_msg result) separator2)

let seed = ref ~-1
let st = ref None

(* initialize random generator from seed (if any) *)
let random_state () = match !st with
  | None -> assert false
  | Some st -> st

let setup_random_state_ () =
  let s = if !seed = ~-1 then (
      Random.self_init ();  (* make new, truly random seed *)
      Random.int (1 lsl 29);
  ) else !seed in
  seed := s;
  Printf.printf "random seed: %d\n%!" s;
  st := Some (Random.State.make [| s |])

(* Function which runs the given function and returns the running time
   of the function, and the original result in a tuple *)
let time_fun f x y =
  let begin_time = Unix.gettimeofday () in
  let res = f x y in (* evaluate this first *)
  Unix.gettimeofday () -. begin_time, res

let run test =
  let print_list = ref false in
  let set_list () = Quickcheck.verbose := true; print_list := true in
  let options = Arg.align
    [ "-v", Arg.Set Quickcheck.verbose, " "
    ; "--verbose", Arg.Set Quickcheck.verbose, " enable verbose tests"
    ; "-l", Arg.Unit set_list, " "
    ; "--list", Arg.Unit set_list, " print list of tests (2 lines each). Implies -verbose"
    ; "-s", Arg.Set_int seed, " "
    ; "--seed", Arg.Set_int seed, " set random seed (to repeat tests)"
    ] in
  Arg.parse options (fun _ ->()) "run qtest suite";
  setup_random_state_ ();
  let _counter = ref (0,0,0) in (* Success, Failure, Other *)
  let total_tests = test_case_count test in
  let update = function
    | RSuccess _ -> let (s,f,o) = !_counter in _counter := (succ s,f,o)
    | RFailure _ -> let (s,f,o) = !_counter in _counter := (s,succ f,o)
    | _ -> let (s,f,o) = !_counter in _counter := (s,f, succ o)
  in
  (* time each test *)
  let start = ref 0. and stop = ref 0. in
  (* display test as it starts and ends *)
  let display_test ?(ended=false) p  =
    let (s,f,o) = !_counter in
    let cartouche = va " [%d%s%s / %d] " s
      (if f=0 then "" else va "+%d" f)
      (if o=0 then "" else va " %d!" o) total_tests
    and path = string_of_path p in
    let end_marker =
      if !print_list then (
        (* print a single line *)
        if ended then va " (after %.2fs)\n" (!stop -. !start) else "\n"
      ) else (
        ps "\r";
        if ended then " *" else ""
      )
    in
    let line = cartouche ^ path ^ end_marker in
    let remaining = 79 - String.length line in
    let cover = if remaining > 0 && not !print_list
      then String.make remaining ' ' else "" in
    pf "%s%s%!" line cover;
  in
  let hdl_event = function
    | EStart p -> start := Unix.gettimeofday(); display_test p
    | EEnd p  -> stop := Unix.gettimeofday(); display_test p ~ended:true
    | EResult result -> update result
  in
  ps "Running tests...";
  let running_time, results = time_fun perform_test hdl_event test in
  let (_s, f, o) = !_counter in
  let failures = List.filter not_success results in
(*  assert (List.length failures = f);*)
  ps "\r";
  print_result_list failures;
  assert (List.length results = total_tests);
  pf "Ran: %d tests in: %.2f seconds.%s\n"
    total_tests running_time (String.make 40 ' ');
  if failures = [] then pl "SUCCESS";
  if o <> 0 then pl "WARNING! SOME TESTS ARE NEITHER SUCCESSES NOR FAILURES!";
  (* create a meaningful return code for the process running the tests *)
  match f, o with
    | 0, 0 -> 0
    | _ -> 1

(* TAP-compatible test runner, in case we want to use a test harness *)

let run_tap test =
  let test_number = ref 0 in
  let handle_event = function
    | EStart _ | EEnd _ -> incr test_number
    | EResult (RSuccess p) ->
      pf "ok %d - %s\n%!" !test_number (string_of_path p)
    | EResult (RFailure (p,m)) ->
      pf "not ok %d - %s # %s\n%!" !test_number (string_of_path p) m
    | EResult (RError (p,m)) ->
      pf "not ok %d - %s # ERROR:%s\n%!" !test_number (string_of_path p) m
    | EResult (RSkip (p,m)) ->
      pf "not ok %d - %s # skip %s\n%!" !test_number (string_of_path p) m
    | EResult (RTodo (p,m)) ->
      pf "not ok %d - %s # todo %s\n%!" !test_number (string_of_path p) m
  in
  let total_tests = test_case_count test in
  pf "TAP version 13\n1..%d\n" total_tests;
  perform_test handle_event test
