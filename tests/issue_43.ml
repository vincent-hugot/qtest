
(* should always run 100 tests *)

open QCheck;;
let is_even i = (i mod 2 = 0);;
let is_odd i = (i mod 2 = 1);;
let t = Test.make pos_int (fun i -> (is_even i) ==> (is_odd (succ i))) in
QCheck_runner.run_tests ~verbose:true [t];;
