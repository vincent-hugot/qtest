(*
QCheck: Random testing for OCaml
Copyright (C) 2016  Vincent Hugot, Simon Cruanes

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Library General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Library General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

(** {1 Runners for Tests} *)

(** {2 State} *)

val random_state : unit -> Random.State.t
(** Access the current random state *)

val set_seed : int -> unit
(** Change the {!random_state} by creating a new one, initialized with
    the given seed. *)

(** {2 Run functions} *)

val run : ?argv:string array -> OUnit.test -> int
(** [run test] runs the test, and returns an error code  that is [0]
    if all tests passed, [1] otherwise *)

val run_tap : OUnit.test -> OUnit.test_results
(** TAP-compatible test runner, in case we want to use a test harness *)

val run_tests : ?verbose:bool -> ?out:out_channel -> ?rand:Random.State.t ->
                QCheck.Test.t list -> bool
(** Run a suite of tests, and print its results
    @param verbose if true, prints more information about test cases (@since 0.4) *)

val run_main : ?argv:string array -> QCheck.Test.t list -> unit
(** Can be used as the main function of a test file. Exits with a non-0 code
    if the tests fail.
    @since 0.4 *)
