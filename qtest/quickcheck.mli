val (==>) : bool -> bool -> bool
(** [b1 ==> b2] is the logical implication [b1 => b2]
    ie [not b1 || b2] (except that it is strict).
*)

(** {2 Generate Values} *)
module Gen : sig
  type 'a t = Random.State.t -> 'a
  (** A random generator for values of type 'a *)

  type 'a sized = int -> Random.State.t -> 'a
  (** Random generator with a size bound *)

  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t

  val oneof : 'a t list -> 'a t
  val oneofl : 'a list -> 'a t
  val frequency : (int * 'a t) list -> 'a t
  val frequencyl : (int * 'a) list -> 'a t

  val unit: unit t
  val bool: bool t

  val float: float t
  val pfloat : float t (** positive float *)
  val nfloat : float t (** negative float *)

  val nat : int t (** small nat *)
  val neg_int : int t (** negative int *)
  val pint : int t (** positive uniform int *)
  val int : int t (** uniform int *)
  val small_int : int t (** Synonym to {!nat} *)
  val int_bound : int -> int t (** Uniform within [0... bound] *)
  val int_range : int -> int -> int t (** Uniform within [low,high] *)
  val (--) : int -> int -> int t (** Synonym to {!int_range} *)

  val ui32 : int32 t
  val ui64 : int64 t

  val list : 'a t -> 'a list t
  val list_size : int t -> 'a t -> 'a list t

  val array : 'a t -> 'a array t
  val array_size : int t -> 'a t -> 'a array t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val char : char t
  val printable : char t
  val numeral : char t

  val string_size : ?gen:char t -> int t -> string t
  val string : ?gen:char t -> string t

  val sized : 'a sized -> 'a t

  val fix : ('a sized -> 'a sized) -> 'a sized (** Fixpoint; size decreases *)

  (** Example:
  {[
  type tree = Leaf of int | Node of tree * tree

  let leaf x = Leaf x
  let node x y = Node (x,y)

  let g = Quickcheck.Gen.(sized @@ fix
    (fun self n st -> match n with
      | 0 -> map leaf nat st
      | n ->
        frequency
          [1, map leaf nat;
           2, map2 node (self (n/2)) (self (n/2))]
           st
      ))

  ]}

  *)
end

(** {2 Show Values} *)
module Print : sig
  type 'a t = 'a -> string

  val int : int t
  val bool : bool t
  val float : float t
  val char : char t
  val string : string t
  val option : 'a t -> 'a option t

  val pair : 'a t -> 'b t -> ('a*'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a*'b*'c) t
  val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a*'b*'c*'d) t

  val list : 'a t -> 'a list t
  val array : 'a t -> 'a array t
end

module Iter : sig
  type 'a t = ('a -> unit) -> unit

  val empty : 'a t
  val return : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val append : 'a t -> 'a t -> 'a t
  val (<+>) : 'a t -> 'a t -> 'a t (** Synonym to {!append} *)
  val of_list : 'a list -> 'a t
  val of_array : 'a array -> 'a t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val find : ('a -> bool) -> 'a t -> 'a option
end

(** {2 Shrink Values} *)
module Shrink : sig
  type 'a t = 'a -> 'a Iter.t

  val nil : 'a t
  (** No shrink *)

  val option : 'a t -> 'a option t
  val string : string t
  val array : ?shrink:'a t -> 'a array t
  val list : ?shrink:'a t -> 'a list t

  val pair : 'a t -> 'b t -> ('a * 'b) t
  val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
end

type 'a arbitrary = {
  gen: 'a Gen.t;
  print: ('a -> string) option; (** print values *)
  small: ('a -> int) option;  (** size of example *)
  shrink: ('a Shrink.t) option;  (** shrink to smaller examples *)
  collect: ('a -> string) option;  (** map value to tag, and group by tag *)
}
(** a value of type ['a arbitrary] is an object with a method for generating random
    values of type ['a], and additional methods to compute the size of values,
    print them, and possibly shrink them into smaller counterexamples
*)

val make :
  ?print:'a Print.t ->
  ?small:('a -> int) ->
  ?shrink:'a Shrink.t ->
  ?collect:('a -> string) ->
  'a Gen.t -> 'a arbitrary
(** Builder for arbitrary. Default is to only have a generator, but other
    arguments can be added *)

val set_print : 'a Print.t -> 'a arbitrary -> 'a arbitrary
val set_small : ('a -> int) -> 'a arbitrary -> 'a arbitrary
val set_shrink : 'a Shrink.t -> 'a arbitrary -> 'a arbitrary
val set_collect : ('a -> string) -> 'a arbitrary -> 'a arbitrary

val choose : 'a arbitrary list -> 'a arbitrary
(** Choose among the given list of generators. The list must not
  be empty; if it is Invalid_argument is raised. *)

val unit : unit arbitrary
(** always generates [()], obviously. *)

val bool : bool arbitrary
(** uniform boolean generator *)

val float : float arbitrary
(* FIXME: does not generate nan nor infinity I think *)
(** generates regular floats (no nan and no infinities) *)

val pos_float : float arbitrary
(** positive float generator (no nan and no infinities) *)

val neg_float : float arbitrary
(** negative float generator (no nan and no infinities) *)

val int : int arbitrary
(** int generator. Uniformly distributed *)

val int_bound : int -> int arbitrary
(** [int_bound n] is uniform between [0] and [n] included *)

val int_range : int -> int -> int arbitrary
(** [int_range a b] is uniform between [a] and [b] included. [b] must be
    larger than [a]. *)

val (--) : int -> int -> int arbitrary
(** Synonym to {!int_range} *)

val int32 : int32 arbitrary
(** int32 generator. Uniformly distributed *)

val int64 : int64 arbitrary
(** int generator. Uniformly distributed *)

val pos_int : int arbitrary
(** positive int generator. Uniformly distributed *)

val small_int : int arbitrary
(** positive int generator. The probability that a number is chosen
    is roughly an exponentially decreasing function of the number.
*)

val small_int_corners : unit -> int arbitrary
(** As [small_int], but each newly created generator starts with
 a list of corner cases before falling back on random generation. *)

val neg_int : int arbitrary
(** negative int generator. The distribution is similar to that of
    [small_int], not of [pos_int].
*)

val char : char arbitrary
(** Uniformly distributed on all the chars (not just ascii or
    valid latin-1) *)

val printable_char : char arbitrary
(* FIXME: describe which subset *)
(** uniformly distributed over a subset of chars *)

val numeral_char : char arbitrary
(** uniformy distributed over ['0'..'9'] *)

val string_gen_of_size : int Gen.t -> char Gen.t -> string arbitrary

val string_gen : char Gen.t -> string arbitrary
(** generates strings with a distribution of length of [small_int] *)

val string : string arbitrary
(** generates strings with a distribution of length of [small_int]
    and distribution of characters of [char] *)

val string_of_size : int Gen.t -> string arbitrary
(** generates strings with distribution of characters if [char] *)

val printable_string : string arbitrary
(** generates strings with a distribution of length of [small_int]
    and distribution of characters of [printable_char] *)

val printable_string_of_size : int Gen.t -> string arbitrary
(** generates strings with distribution of characters of [printable_char] *)

val numeral_string : string arbitrary
(** generates strings with a distribution of length of [small_int]
    and distribution of characters of [numeral_char] *)

val numeral_string_of_size : int Gen.t -> string arbitrary
(** generates strings with a distribution of characters of [numeral_char] *)

val list : 'a arbitrary -> 'a list arbitrary
(** generates lists with length generated by [small_int] *)

val list_of_size : int Gen.t -> 'a arbitrary -> 'a list arbitrary
(** generates lists with length from the given distribution *)

val array : 'a arbitrary -> 'a array arbitrary
(** generates arrays with length generated by [small_int] *)

val array_of_size : int Gen.t -> 'a arbitrary -> 'a array arbitrary
(** generates arrays with length from the given distribution *)

val pair : 'a arbitrary -> 'b arbitrary -> ('a * 'b) arbitrary
(** combines two generators into a generator of pairs *)

val triple : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> ('a * 'b * 'c) arbitrary
(** combines three generators into a generator of 3-uples *)

val option : 'a arbitrary -> 'a option arbitrary
(** choose between returning Some random value, or None *)

val fun1 : 'a arbitrary -> 'b arbitrary -> ('a -> 'b) arbitrary
(** generator of functions of arity 1.
    The functions are always pure and total functions:
    - when given the same argument (as decided by Pervasives.(=)), it returns the same value
    - it never does side effects, like printing or never raise exceptions etc.
    The functions generated are really printable.
*)

val fun2 : 'a arbitrary -> 'b arbitrary -> 'c arbitrary -> ('a -> 'b -> 'c) arbitrary
(** generator of functions of arity 2. The remark about [fun1] also apply
    here.
*)

val oneofl : ?print:'a Print.t -> ?collect:('a -> string) ->
             'a list -> 'a arbitrary
(** Pick an element randomly in the list *)

val oneof : 'a arbitrary list -> 'a arbitrary
(** Pick a generator among the list, randomly *)

val always : ?print:'a Print.t -> 'a -> 'a arbitrary
(** Always return the same element *)

val frequency : ?print:'a Print.t -> ?small:('a -> int) ->
                ?shrink:'a Shrink.t -> ?collect:('a -> string) ->
                (int * 'a arbitrary) list -> 'a arbitrary
(** Similar to {!oneof} but with frequencies *)

val frequencyl : ?print:'a Print.t -> ?small:('a -> int) ->
                (int * 'a) list -> 'a arbitrary
(** Same as {!oneofl}, but each element is paired with its frequency in
    the probability distribution (the higher, the more likely) *)

val map : ?rev:('b -> 'a) -> ('a -> 'b) -> 'a arbitrary -> 'b arbitrary
(** [map f a] returns a new arbitrary instance that generates values using
    [a#gen] and then transforms them through [f].
    @param rev if provided, maps values back to type ['a] so that the printer,
      shrinker, etc. of [a] can be used
*)

val map_same_type : ('a -> 'a) -> 'a arbitrary -> 'a arbitrary
(** Specialization of [map] when the transformation preserves the type, which
   makes shrinker, printer, etc. still relevant *)

val verbose : bool ref
(** Default is [false], but if [true], random tests will  print statistics
    on their set of inputs *)

val laws_exn :
  ?small:('a -> int) -> ?count:int -> ?max_gen:int -> ?max_fail:int ->
  string -> 'a arbitrary -> ('a -> bool) -> Random.State.t -> unit
 (** [laws_exn ?small ?count name arbitrary law st] generates up to [count] random
     values of type ['a] with using [arbitrary] and the random state [st]. The
     predicate [law] is called on them and if it returns [false] or raises an
     exception then we have a counter example for the [law].

     @param small kept for compatibility reasons; if provided, replaces
      the field [arbitrary.small].
     @param max_gen maximum number of times the generation function is called
      to replace inputs that do not satisfy preconditions
     @param max_fail maximum number of failures before we stop generating
      inputs. This is useful if shrinking takes too much time.

     @raise Failure if a counter example has been found, containing the stringified example
 *)
