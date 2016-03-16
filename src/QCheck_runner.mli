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

(** {2 Conversion of tests to OUnit Tests} *)

val to_ounit_test : QCheck.test -> OUnit.test
(** [to_ounit_test t] wraps [t] into a OUnit test, picking a fresh name
    if the  test did not provide any *)

val (>:::) : string -> QCheck.suite -> OUnit.test

val (~::) : QCheck.test -> OUnit.test
(** [~:: test] converts [test] into an OUnit test *)

(** {2 Run functions} *)

val run : OUnit.test -> int
(** [run test] runs the test, and returns an error code  that is [0]
    if all tests passed, [1] otherwise *)

val run_tap : OUnit.test -> OUnit.test_results
(** TAP-compatible test runner, in case we want to use a test harness *)
