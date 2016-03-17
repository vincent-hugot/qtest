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

(** {1 Quickcheck inspired property-based testing} *)

open Printf

module RS = Random.State

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let _is_some = function Some _ -> true | None -> false

let _opt_map_or ~d ~f = function
  | None -> d
  | Some x -> f x

let _opt_or a b = match a with
  | None -> b
  | Some x -> x

let _opt_map ~f = function
  | None -> None
  | Some x -> Some (f x)

let _opt_map_2 ~f a b = match a, b with
  | Some x, Some y -> Some (f x y)
  | _ -> None

let _opt_map_3 ~f a b c = match a, b, c with
  | Some x, Some y, Some z -> Some (f x y z)
  | _ -> None

let _opt_sum a b = match a, b with
  | Some _, _ -> a
  | None, _ -> b

let sum_int = List.fold_left (+) 0

exception FailedPrecondition
(* raised if precondition is false *)

let (==>) b1 b2 = if b1 then b2 else raise FailedPrecondition

module Gen = struct
  type 'a t = RS.t -> 'a
  type 'a sized = int -> Random.State.t -> 'a

  let return x _st = x

  let (>>=) gen f st =
    f (gen st) st

  let (<*>) f x st = f st (x st)
  let map f x st = f (x st)
  let map2 f x y st = f (x st) (y st)
  let map3 f x y z st = f (x st) (y st) (z st)
  let (>|=) x f = map f x

  let oneof l st = List.nth l (Random.State.int st (List.length l)) st
  let oneofl xs st = List.nth xs (Random.State.int st (List.length xs))
  let oneofa xs st = Array.get xs (Random.State.int st (Array.length xs))

  let frequencyl l st =
    let sums = sum_int (List.map fst l) in
    let i = Random.State.int st sums in
    let rec aux acc = function
      | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs
      | _ -> failwith "frequency"
    in
    aux 0 l

  let frequencya a = frequencyl (Array.to_list a)

  let frequency l st = frequencyl l st st

  (* natural number generator *)
  let nat st =
    let p = RS.float st 1. in
    if p < 0.5 then RS.int st 10
    else if p < 0.75 then RS.int st 100
    else if p < 0.95 then RS.int st 1_000
    else RS.int st 10_000

  let small_int = nat

  let unit _st = ()

  let bool st = RS.bool st

  let float st =
    exp (RS.float st 15. *. (if RS.float st 1. < 0.5 then 1. else -1.))
    *. (if RS.float st 1. < 0.5 then 1. else -1.)

  let pfloat st = abs_float (float st)
  let nfloat st = -.(pfloat st)

  let neg_int st = -(nat st)

  (* Uniform random int generator *)
  let pint =
    if Sys.word_size = 32 then
      fun st -> RS.bits st
    else (* word size = 64 *)
      fun st ->
        RS.bits st                        (* Bottom 30 bits *)
        lor (RS.bits st lsl 30)           (* Middle 30 bits *)
        lor ((RS.bits st land 3) lsl 60)  (* Top 2 bits *)  (* top bit = 0 *)

  let int st = if RS.bool st then - (pint st) - 1 else pint st
  let int_bound n =
    assert (n >= 0);
    fun st -> Random.State.int st (n+1)
  let int_range a b =
    assert (b >= a);
    fun st -> a + (Random.State.int st (b-a+1))
  let (--) = int_range

  let random_binary_string st length =
    (* 0b011101... *)
    let s = Bytes.create (length + 2) in
    Bytes.set s 0 '0';
    Bytes.set s 1 'b';
    for i = 0 to length - 1 do
      Bytes.set s (i+2) (if RS.bool st then '0' else '1')
    done;
    Bytes.unsafe_to_string s

  let ui32 st = Int32.of_string (random_binary_string st 32)
  let ui64 st = Int64.of_string (random_binary_string st 64)

  let list_size size gen st =
    foldn ~f:(fun acc _ -> (gen st)::acc) ~init:[] (size st)
  let list gen st = list_size nat gen st

  let array_size size gen st =
    Array.init (size st) (fun _ -> gen st)
  let array gen st = array_size nat gen st

  let pair gen1 gen2 st = (gen1 st, gen2 st)

  let triple g1 g2 g3 st = (g1 st,g2 st, g3 st)

  let char st = char_of_int (RS.int st 255)

  let printable_chars =
    let l = 126-32+1 in
    let s = Bytes.create l in
    for i = 0 to l-2 do
      Bytes.set s i (char_of_int (32+i))
    done;
    Bytes.set s (l-1) '\n';
    Bytes.unsafe_to_string s

  let printable st = printable_chars.[RS.int st (String.length printable_chars)]
  let numeral st = char_of_int (48 + RS.int st 10)

  let string_size ?(gen = char) size st =
    let s = Bytes.create (size st) in
    for i = 0 to String.length s - 1 do
      Bytes.set s i (gen st)
    done;
    Bytes.unsafe_to_string s
  let string ?gen st = string_size ?gen nat st

  (* corner cases *)

  let graft_corners gen corners () =
    let cors = ref corners in fun st ->
      match !cors with [] -> gen st
      | e::l -> cors := l; e

  let nng_corners () = graft_corners nat [0;1;2;max_int] ()

  (* sized, fix *)

  let sized f st =
    let n = nat st in
    f n st

  let fix f =
    let rec f' n st = f f' n st in
    f'
end

module Print = struct
  type 'a t = 'a -> string

  let int = string_of_int
  let bool = string_of_bool
  let float = string_of_float
  let string s = s
  let char c = String.make 1 c

  let option f = function
    | None -> "None"
    | Some x -> "Some (" ^ f x ^ ")"

  let pair a b (x,y) = Printf.sprintf "(%s, %s)" (a x) (b y)
  let triple a b c (x,y,z) = Printf.sprintf "(%s, %s, %s)" (a x) (b y) (c z)
  let quad a b c d (x,y,z,w) =
    Printf.sprintf "(%s, %s, %s, %s)" (a x) (b y) (c z) (d w)

  let list pp l =
    let b = Buffer.create 25 in
    Buffer.add_char b '[';
    List.iteri (fun i x ->
      if i > 0 then Buffer.add_string b "; ";
      Buffer.add_string b (pp x))
      l;
    Buffer.add_char b ']';
    Buffer.contents b

  let array pp a =
    let b = Buffer.create 25 in
    Buffer.add_string b "[|";
    Array.iteri (fun i x ->
      if i > 0 then Buffer.add_string b "; ";
      Buffer.add_string b (pp x))
      a;
    Buffer.add_string b "|]";
    Buffer.contents b
end

module Iter = struct
  type 'a t = ('a -> unit) -> unit
  let empty _ = ()
  let return x yield = yield x
  let (<*>) a b yield = a (fun f -> b (fun x ->  yield (f x)))
  let (>>=) a f yield = a (fun x -> f x yield)
  let map f a yield = a (fun x -> yield (f x))
  let map2 f a b yield = a (fun x -> b (fun y -> yield (f x y)))
  let (>|=) a f = map f a
  let append a b yield = a yield; b yield
  let (<+>) = append
  let of_list l yield = List.iter yield l
  let of_array a yield = Array.iter yield a
  let pair a b yield = a (fun x -> b(fun y -> yield (x,y)))
  let triple a b c yield = a (fun x -> b (fun y -> c (fun z -> yield (x,y,z))))

  exception IterExit
  let find p iter =
    let r = ref None in
    (try iter (fun x -> if p x then (r := Some x; raise IterExit))
     with IterExit -> ()
    );
    !r
end

module Shrink = struct
  type 'a t = 'a -> 'a Iter.t

  let nil _ = Iter.empty

  (* get closer to 0 *)
  let int x yield =
    if x < -2 || x>2 then yield (x/2); (* faster this way *)
    if x>0 then yield (x-1);
    if x<0 then yield (x+1)

  let option s x = match x with
    | None -> Iter.empty
    | Some x -> Iter.(return None <+> map (fun y->Some y) (s x))

  let string s yield =
    for i =0 to String.length s-1 do
      let s' = Bytes.init (String.length s-1)
        (fun j -> if j<i then s.[j] else s.[j+1])
      in
      yield (Bytes.unsafe_to_string s')
    done

  let array ?shrink a yield =
    for i=0 to Array.length a-1 do
      let a' = Array.init (Array.length a-1)
        (fun j -> if j< i then a.(j) else a.(j+1))
      in
      yield a'
    done;
    match shrink with
    | None -> ()
    | Some f ->
        (* try to shrink each element of the array *)
        for i = 0 to Array.length a - 1 do
          f a.(i) (fun x ->
            let b = Array.copy a in
            b.(i) <- x;
            yield b
          )
        done

  let list ?shrink l yield =
    let rec aux l r = match r with
      | [] -> ()
      | x :: r' ->
          yield (List.rev_append l r');
          aux (x::l) r'
    in
    aux [] l;
    match shrink with
    | None -> ()
    | Some f ->
        let rec aux l r = match r with
          | [] -> ()
          | x :: r' ->
              f x (fun x' -> yield (List.rev_append l (x' :: r')));
              aux (x :: l) r'
        in
        aux [] l

  let pair a b (x,y) yield =
    a x (fun x' -> yield (x',y));
    b y (fun y' -> yield (x,y'))

  let triple a b c (x,y,z) yield =
    a x (fun x' -> yield (x',y,z));
    b y (fun y' -> yield (x,y',z));
    c z (fun z' -> yield (x,y,z'))
end

module Arbitrary = struct
  type 'a t = Random.State.t -> 'a

  let return x st = x

  let int n st = Random.State.int st n

  let int_range ~start ~stop st =
    let n = stop - start in
    if n <= 0
      then 0
      else start + Random.State.int st n

  let (--) start stop = int_range ~start ~stop

  let small_int = int 100

  let split_int gen st =
    let n = gen st in
    if n > 0
      then let i = Random.State.int st (n+1) in i, n-i
      else 0, 0

  let bool = Random.State.bool

  let float f st = Random.State.float st f

  let char st = Char.chr (Random.State.int st 128)

  let alpha st =
    Char.chr (Char.code 'a' + Random.State.int st (Char.code 'z' - Char.code 'a'))

  let string_len len st =
    let n = len st in
    assert (n>=0);
    let b = Buffer.create n in
    for _i = 0 to n-1 do
      Buffer.add_char b (alpha st)
    done;
    Buffer.contents b

  let string st = string_len (int 10) st

  let map ar f st = f (ar st)

  let map' f ar st = f (ar st)

  let rec _make_list ar st acc n =
    if n = 0 then acc else
      let x = ar st in
      _make_list ar st (x::acc) (n-1)

  let list ?(len=int 10) ar st =
    let n = len st in
    _make_list ar st [] n

  let opt ar st =
    if Random.State.bool st
      then Some (ar st)
      else None

  let list_repeat len ar st =
    _make_list ar st [] len

  let array ?(len=int 10) ar st =
    let n = len st in
    Array.init n (fun _ -> ar st)

  let array_repeat n ar st =
    Array.init n (fun _ -> ar st)

  let among_array a st =
    if Array.length a < 1
      then failwith "Arbitrary.among: cannot choose in empty array ";
    let i = Random.State.int st (Array.length a) in
    a.(i)

  let shuffle a st =
    for i = Array.length a-1 downto 1 do
      let j = Random.State.int st (i+1) in
      let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp;
    done

  let among l =
    if List.length l < 1
      then failwith "Arbitrary.among: cannot choose in empty list";
    among_array (Array.of_list l)

  let choose l = match l with
    | [] -> failwith "cannot choose from empty list"
    | [x] -> x
    | _ ->
      let a = Array.of_list l in
      fun st ->
        let i = Random.State.int st (Array.length a) in
        a.(i) st

  let (|||) a b st =
    if Random.State.bool st
      then a st
      else b st

  let fix ?(max=15) ~base f =
    let rec ar = lazy
      (fun depth st ->
        if depth >= max || Random.State.int st max < depth
          then base st (* base case. The deeper, the more likely. *)
        else  (* recurse *)
          let ar' = Lazy.force ar (depth+1) in
          f ar' st)
    in
    Lazy.force ar 0

  let fix_depth ~depth ~base f st =
    let max = depth st in
    fix ~max ~base f st

  (* split fuel into two parts *)
  let split_fuel fuel st =
    let i = Random.State.int st (fuel+1) in
    i, fuel-i

  type 'a recursive_case =
    [ `Base of 'a t
    | `Base_fuel of (int -> 'a t)
    | `Rec of ((int -> 'a list t) -> 'a t)
    | `Rec_fuel of ((int -> 'a list t) -> int -> 'a t)
    | `Rec1 of ('a t -> 'a t)
    | `Rec2 of ('a t -> 'a t -> 'a t)
    ]

  let split_fuel_n n fuel st =
    if fuel<0 || n < 1 then invalid_arg "split_int_n";
    let rec split_rec n fuel acc = match n with
      | 0 -> assert false
      | 1 -> fuel::acc
      | _ ->
          let left, right = split_fuel fuel st in
          (* divide and conquer *)
          let acc = split_rec (n/2) left acc in
          let acc = split_rec (n - n/2) right acc in
          acc
    in
    let l = split_rec n fuel [] in
    assert (List.length l = n);
    l

  exception RecursiveCallFailed

  let fail_fix() = raise RecursiveCallFailed

  let fix_fuel l =
    assert (l<>[]);
    let a = Array.of_list l in
    (* fixpoint. Each element of [l] can ask for a given number of sub-cases
      but only ONCE. *)
    let rec fix fuel st =
      shuffle a st;
      first fuel 0 st
    (* try each possibility. Each possibility must call exactly once
      [fix] recursively on [n] cases ([n=0] for base cases) *)
    and first fuel i st =
      if i=Array.length a then raise RecursiveCallFailed
      else
        let fix' =
          let is_first=ref true in
          fun num st ->
            if not !is_first
              then failwith "fix_fuel: sub_case can be called only once";
            is_first := false;

            match fuel, num with
            | 0, 0 -> []
            | _, 0 -> raise RecursiveCallFailed (* didn't consume enough *)
            | 0, _ -> raise RecursiveCallFailed (* not enough fuel *)
            | _ ->
                (* split fuel for subcases *)
                assert (fuel>0);
                (* if num>=fuel then raise RecursiveCallFailed; *)
                let fuels = split_fuel_n num (fuel-1) st in
                List.map (fun f -> fix f st) fuels
        in
        try
          match a.(i) with
          | `Base f when fuel=0 -> f st
          | `Base _ -> raise RecursiveCallFailed (* didn't consume enough *)
          | `Base_fuel f -> f fuel st (* yield *)
          | `Rec f -> f fix' st
          | `Rec_fuel f -> f fix' (fuel-1) st
          | `Rec1 _ when fuel=0 -> raise RecursiveCallFailed
          | `Rec1 f -> f (fix (fuel-1)) st
          | `Rec2 _ when fuel<2 -> raise RecursiveCallFailed
          | `Rec2 f ->
              let fuel1, fuel2 = split_fuel (fuel-1) st in
              f (fix fuel1) (fix fuel2) st
        with RecursiveCallFailed ->
          first fuel (i+1) st  (* try next *)
    in
    fun fuel st ->
      try Some (fix fuel st)
      with RecursiveCallFailed -> None

  type ('a, 'state) general_recursive_case =
    [ `Base of ('state -> 'a t)
    | `Base_fuel of (int -> 'state -> 'a t)
    | `Rec of ((int -> ('state -> 'a) list t) -> 'state -> 'a t)
    | `Rec_fuel of ((int -> ('state -> 'a) list t) -> int -> 'state -> 'a t)
    | `Rec1 of (('state -> 'a t) -> 'state -> 'a t)
    | `Rec2 of (('state -> 'a t) -> ('state -> 'a t) -> 'state -> 'a t)
    ]

  let fix_fuel_gen (l:('a,'state) general_recursive_case list) =
    assert (l<>[]);
    let a = Array.of_list l in
    (* fixpoint. Each element of [l] can ask for a given number of sub-cases
      but only ONCE. *)
    let rec fix fuel state st =
      shuffle a st;
      first fuel state 0 st
    (* try each possibility. Each possibility must call exactly once
      [fix] recursively on [n] cases ([n=0] for base cases) *)
    and first fuel state i st =
      if i=Array.length a then raise RecursiveCallFailed
      else
        let fix' =
          let is_first=ref true in
          fun num st ->
            if not !is_first
              then failwith "fix_fuel: sub_case can be called only once";
            is_first := false;

            match fuel, num with
            | 0, 0 -> []
            | _, 0 -> raise RecursiveCallFailed (* didn't consume enough *)
            | 0, _ -> raise RecursiveCallFailed (* not enough fuel *)
            | _ ->
                (* split fuel for subcases *)
                assert (fuel>0);
                if num >= fuel then raise RecursiveCallFailed;
                let fuels = split_fuel_n num (fuel-1) st in
                List.map (fun f state -> fix f state st) fuels
        in
        try
          match a.(i) with
          | `Base f when fuel=0 -> f state st
          | `Base _ -> raise RecursiveCallFailed (* didn't consume enough *)
          | `Base_fuel f -> f fuel state st (* yield *)
          | `Rec f -> f fix' state st
          | `Rec_fuel f -> f fix' fuel state st
          | `Rec1 _ when fuel=0 -> raise RecursiveCallFailed
          | `Rec1 f -> f (fix (fuel-1)) state st
          | `Rec2 _ when fuel<2 -> raise RecursiveCallFailed
          | `Rec2 f ->
              let fuel1, fuel2 = split_fuel (fuel-1) st in
              f (fix fuel1) (fix fuel2) state st
        with RecursiveCallFailed ->
          first fuel state (i+1) st  (* try next *)
    in
    fun fuel state st ->
      try Some (fix fuel state st)
      with RecursiveCallFailed -> None

  let rec retry gen st = match gen st with
    | None -> retry gen st
    | Some x -> x

  let lift f a st = f (a st)

  let lift2 f a b st = f (a st) (b st)

  let lift3 f a b c st = f (a st) (b st) (c st)

  let lift4 f a b c d st = f (a st) (b st) (c st) (d st)

  let pair a b = lift2 (fun x y -> x,y) a b

  let triple a b c = lift3 (fun x y z -> x,y,z) a b c

  let quad a b c d = lift4 (fun x y z w -> x,y,z,w) a b c d

  let (>>=) a f st =
    let x = a st in
    f x st

  let (>|=) a f st = f (a st)

  let (<*>) f x st = f st (x st)

  let pure x _st = x

  let generate ?(n=100) ?(rand=Random.State.make_self_init()) gen =
    let l = ref [] in
    for i = 0 to n-1 do
      l := (gen rand) :: !l
    done;
    !l
end

type 'a arbitrary = {
  gen: 'a Gen.t;
  print: ('a -> string) option; (** print values *)
  small: ('a -> int) option;  (** size of example *)
  shrink: ('a -> 'a Iter.t) option;  (** shrink to smaller examples *)
  collect: ('a -> string) option;  (** map value to tag, and group by tag *)
}

let make ?print ?small ?shrink ?collect gen = {
  gen;
  print;
  small;
  shrink;
  collect;
}

let set_small f o = {o with small=Some f}
let set_print f o = {o with print=Some f}
let set_shrink f o = {o with shrink=Some f}
let set_collect f o = {o with collect=Some f}

let small1 _ = 1

let make_scalar ?print ?collect gen =
  make ~shrink:Shrink.nil ~small:small1 ?print ?collect gen
let make_int ?collect gen =
  make ~shrink:Shrink.int ~small:small1 ~print:Print.int ?collect gen

let adapt_ o gen =
  make ?print:o.print ?small:o.small ?shrink:o.shrink ?collect:o.collect gen

let choose l = match l with
  | [] -> raise (Invalid_argument "quickcheck.choose")
  | l ->
      let a = Array.of_list l in
      adapt_ a.(0)
        (fun st ->
          let arb = a.(RS.int st (Array.length a)) in
          arb.gen st)

let unit : unit arbitrary =
  make ~small:small1 ~shrink:Shrink.nil ~print:(fun _ -> "()") Gen.unit

let bool = make_scalar ~print:string_of_bool Gen.bool
let float = make_scalar ~print:string_of_float Gen.float
let pos_float = make_scalar ~print:string_of_float Gen.pfloat
let neg_float = make_scalar ~print:string_of_float Gen.nfloat

let int = make_int Gen.int
let int_bound n = make_int (Gen.int_bound n)
let int_range a b = make_int (Gen.int_range a b)
let (--) = int_range
let pos_int = make_int Gen.pint
let small_int = make_int Gen.nat
let small_int_corners () = make_int (Gen.nng_corners ())
let neg_int = make_int Gen.neg_int

let int32 = make_scalar ~print:(fun i -> Int32.to_string i ^ "l") Gen.ui32
let int64 = make_scalar ~print:(fun i -> Int64.to_string i ^ "L") Gen.ui64

let char = make_scalar ~print:(sprintf "%C") Gen.char
let printable_char = make_scalar ~print:(sprintf "%C") Gen.printable
let numeral_char = make_scalar ~print:(sprintf "%C") Gen.numeral

let string_gen_of_size size gen =
  make ~shrink:Shrink.string ~small:String.length
    ~print:(sprintf "%S") (Gen.string_size ~gen size)
let string_gen gen =
  make ~shrink:Shrink.string ~small:String.length
    ~print:(sprintf "%S") (Gen.string ~gen)

let string = string_gen Gen.char
let string_of_size size = string_gen_of_size size Gen.char

let printable_string = string_gen Gen.printable
let printable_string_of_size size = string_gen_of_size size Gen.printable

let numeral_string = string_gen Gen.numeral
let numeral_string_of_size size = string_gen_of_size size Gen.numeral

let list_sum_ f l = List.fold_left (fun acc x-> f x+acc) 0 l

let list a =
  (* small sums sub-sizes if present, otherwise just length *)
  let small = _opt_map_or a.small ~f:list_sum_ ~d:List.length in
  let print = _opt_map a.print ~f:Print.list in
  make
    ~small
    ~shrink:(Shrink.list ?shrink:a.shrink)
    ?print
    (Gen.list a.gen)

let list_of_size size a =
  let small = _opt_map_or a.small ~f:list_sum_ ~d:List.length in
  let print = _opt_map a.print ~f:Print.list in
  make
    ~small
    ~shrink:(Shrink.list ?shrink:a.shrink)
    ?print
    (Gen.list_size size a.gen)

let array_sum_ f a = Array.fold_left (fun acc x -> f x+acc) 0 a

let array a =
  let small = _opt_map_or ~d:Array.length ~f:array_sum_ a.small in
  make
    ~small
    ~shrink:(Shrink.array ?shrink:a.shrink)
    ?print:(_opt_map ~f:Print.array a.print)
    (Gen.array a.gen)

let array_of_size size a =
  let small = _opt_map_or ~d:Array.length ~f:array_sum_ a.small in
  make
    ~small
    ~shrink:(Shrink.array ?shrink:a.shrink)
    ?print:(_opt_map ~f:Print.array a.print)
    (Gen.array_size size a.gen)

let pair a b =
  make
    ?small:(_opt_map_2 ~f:(fun f g (x,y) -> f x+g y) a.small b.small)
    ?print:(_opt_map_2 ~f:Print.pair a.print b.print)
    ~shrink:(Shrink.pair (_opt_or a.shrink Shrink.nil) (_opt_or b.shrink Shrink.nil))
    (Gen.pair a.gen b.gen)

let triple a b c =
  make
    ?small:(_opt_map_3 ~f:(fun f g h (x,y,z) -> f x+g y+h z) a.small b.small c.small)
    ?print:(_opt_map_3 ~f:Print.triple a.print b.print c.print)
    ~shrink:(Shrink.triple (_opt_or a.shrink Shrink.nil)
      (_opt_or b.shrink Shrink.nil) (_opt_or c.shrink Shrink.nil))
    (Gen.triple a.gen b.gen c.gen)

let option a =
  let g f st =
    let p = RS.float st 1. in
    if p < 0.15 then None
    else Some (f st)
  and shrink = _opt_map a.shrink ~f:Shrink.option
  and small =
    _opt_map_or a.small ~d:(function None -> 0 | Some _ -> 1)
      ~f:(fun f o -> match o with None -> 0 | Some x -> f x)
  in
  make
    ~small
    ?shrink
    ?print:(_opt_map ~f:Print.option a.print)
    (g a.gen)

(* TODO: explain black magic in this!! *)
let fun1 : 'a arbitrary -> 'b arbitrary -> ('a -> 'b) arbitrary =
  fun a1 a2 ->
    let magic_object = Obj.magic (object end) in
    let gen : ('a -> 'b) Gen.t = fun st ->
      let h = Hashtbl.create 10 in
      fun x ->
        if x == magic_object then
          Obj.magic h
        else
          try Hashtbl.find h x
          with Not_found ->
            let b = a2.gen st in
            Hashtbl.add h x b;
            b in
    let pp : (('a -> 'b) -> string) option = _opt_map_2 a1.print a2.print ~f:(fun p1 p2 f ->
      let h : ('a, 'b) Hashtbl.t = Obj.magic (f magic_object) in
      let b = Buffer.create 20 in
      Hashtbl.iter (fun key value -> Printf.bprintf b "%s -> %s; " (p1 key) (p2 value)) h;
      "{" ^ Buffer.contents b ^ "}"
    ) in
    make
      ?print:pp
      gen

let fun2 gp1 gp2 gp3 = fun1 gp1 (fun1 gp2 gp3)

(* Generator combinators *)

(** given a list, returns generator that picks at random from list *)
let oneofl ?print ?collect xs = make ?print ?collect (Gen.oneofl xs)
let oneofa ?print ?collect xs = make ?print ?collect (Gen.oneofa xs)

(** Given a list of generators, returns generator that randomly uses one of the generators
    from the list *)
let oneof l =
  let gens = List.map (fun a->a.gen) l in
  let first = List.hd l in
  let print = first.print
  and small = first.small
  and collect = first.collect
  and shrink = first.shrink in
  make ?print ?small ?collect ?shrink (Gen.oneof gens)

(** Generator that always returns given value *)
let always ?print x =
  let gen _st = x in
  make ?print gen

(** like oneof, but with weights *)
let frequency ?print ?small ?shrink ?collect l =
  let first = snd (List.hd l) in
  let small = _opt_sum small first.small in
  let print = _opt_sum print first.print in
  let shrink = _opt_sum shrink first.shrink in
  let collect = _opt_sum collect first.collect in
  let gens = List.map (fun (x,y) -> x, y.gen) l in
  make ?print ?small ?shrink ?collect (Gen.frequency gens)

(** Given list of [(frequency,value)] pairs, returns value with probability proportional
    to given frequency *)
let frequencyl ?print ?small l = make ?print ?small (Gen.frequencyl l)
let frequencya ?print ?small l = make ?print ?small (Gen.frequencya l)

let map ?rev f a =
  make
    ?print:(_opt_map_2 rev a.print ~f:(fun r p x -> p (r x)))
    ?small:(_opt_map_2 rev a.small ~f:(fun r s x -> s (r x)))
    ?shrink:(_opt_map_2 rev a.shrink ~f:(fun r g x -> Iter.(g (r x) >|= f)))
    ?collect:(_opt_map_2 rev a.collect ~f:(fun r f x -> f (r x)))
    (fun st -> f (a.gen st))

let map_same_type f a =
  adapt_ a (fun st -> f (a.gen st))

type 'a failed_state = {
  counter_ex: 'a; (** The counter-example itself *)
  shrink_steps: int; (** How many shrinking steps for this counterex *)
  failures_num: int; (** Number of counter-examples found *)
}

type 'a result_state =
  | Success
  | Failed of 'a failed_state

(* result returned by [laws] *)
type 'a result = {
  mutable res_state : 'a result_state;
  mutable res_count: int;  (* number of tests *)
  mutable res_gen: int; (* number of generated cases *)
  res_collect: (string, int) Hashtbl.t lazy_t;
}

let fail ?small ~steps res i = match res.res_state with
  | Success ->
      res.res_state <- Failed {counter_ex=i; shrink_steps=steps; failures_num=1}
  | Failed f ->
      match small with
      | Some small when small i < small f.counter_ex ->
          res.res_state <- Failed {
            counter_ex=i;
            failures_num=f.failures_num+1;
            shrink_steps=steps;
          }
      | _ ->
          res.res_state <- Failed {f with failures_num=f.failures_num+1}

module LawsState = struct
  (* state required by [laws] to execute *)
  type 'a t = {
    arb: 'a arbitrary;
    func: 'a -> bool;
    st: Random.State.t;
    res: 'a result;
    mutable num: int;  (** number of iterations to do *)
    mutable max_gen: int; (** maximum number of generations allowed *)
    mutable max_fail: int; (** maximum number of counter-examples allowed *)
  }

  let is_done state = state.num <= 0 || state.max_gen <= 0

  let decr_count state =
    state.res.res_count <- state.res.res_count + 1;
    state.num <- state.num - 1

  let new_input state =
    state.res.res_gen <- state.res.res_gen + 1;
    state.max_gen <- state.max_gen - 1;
    state.arb.gen state.st

  (* statistics on inputs *)
  let collect st i = match st.arb.collect with
    | None -> ()
    | Some f ->
        let key = f i in
        let (lazy tbl) = st.res.res_collect in
        let n = try Hashtbl.find tbl key with Not_found -> 0 in
        Hashtbl.replace tbl key (n+1)

  (* try to shrink counter-ex [i] into a smaller one. Returns
     shrinked value and number of steps *)
  let shrink st i =
    let rec shrink_ st i ~steps = match st.arb.shrink with
      | None -> i, steps
      | Some f ->
        let i' = Iter.find
          (fun x ->
            try
              not (st.func x)
            with FailedPrecondition -> false
            | _ -> true (* fail test (by error) *)
          ) (f i)
        in
        match i' with
        | None -> i, steps
        | Some i' -> shrink_ st i' ~steps:(steps+1) (* shrink further *)
    in
    shrink_ ~steps:0 st i
end

module S = LawsState

(** [laws iter gen func] applies [func] repeatedly ([iter] times) on output of [gen], and
    if [func] ever returns false, then the input that caused the failure is returned
    in [Failed].
    If [func input] raises [FailedPrecondition] then  the input is discarded, unless
       max_gen is 0.
    @param max_gen max number of times [gen] is called to replace inputs
      that fail the precondition *)
let rec laws state =
  if S.is_done state then state.S.res
  else
    let input = S.new_input state in
    S.collect state input;
    try
      if state.S.func input
      then (
        (* one test ok *)
        S.decr_count state;
        laws state
      ) else handle_fail state input
    with
    | FailedPrecondition when state.S.max_gen > 0 -> laws state
    | _ -> handle_fail state input
and handle_fail state input =
  (* first, shrink *)
  let input, steps = S.shrink state input in
  (* fail *)
  S.decr_count state;
  state.S.max_fail <- state.S.max_fail - 1;
  fail state.S.res ~steps input;
  if _is_some state.S.arb.small && state.S.max_fail > 0
    then laws state
    else state.S.res

let default_count = 100
let default_max_gen = 300

let no_print_ _ = "<no printer>"

let verbose = ref false

(** Like laws, but throws an exception instead of returning an option.  *)
let laws_exn ?small ?(count=default_count) ?(max_gen=default_max_gen) ?max_fail name a func st =
  let a = match small with None -> a | Some f -> set_small f a in
  let max_fail = match max_fail with None -> count | Some x -> x in
  let state = {S.
    func;
    st;
    arb = a;
    max_gen;
    max_fail;
    num = count;
    res = {
      res_state=Success; res_count=0; res_gen=0;
      res_collect=lazy (Hashtbl.create 10);
    };
  } in
  let result = laws state in
  if !verbose then (
    Printf.printf "\rlaw %s: %d relevant cases (%d total)\n"
      name result.res_count result.res_gen;
    begin match a.collect with
    | None -> ()
    | Some _ ->
        let (lazy tbl) = result.res_collect in
        Hashtbl.iter
          (fun case num -> Printf.printf "\r  %s: %d cases\n" case num)
          tbl
    end;
    flush stderr;
  );
  match result.res_state with
    | Success -> ()
    | Failed f ->
        let pp = match a.print with None -> no_print_ | Some x -> x in
        let msg = Printf.sprintf "law %s failed on %d cases. Ex: %s%s"
          name f.failures_num (pp f.counter_ex)
            (if f.shrink_steps > 0
              then Printf.sprintf " (after %d shrink steps)" f.shrink_steps else "")
        in
        failwith msg



(* XXX cut here *)

module Test = struct
  type 'a result =
    | Ok of int * int  (* total number / precond failed *)
    | Failed of 'a list
    | Error of 'a option * exn

  (* random seed, for repeatability of tests *)
  let __seed = [| 89809344; 994326685; 290180182 |]

  let check ?(call=fun _ _ -> ()) ?(rand=Random.State.make __seed) ?(n=100) gen prop =
    let precond_failed = ref 0 in
    let failures = ref [] in
    let inst = ref None in
    try
      for i = 0 to n - 1 do
        let x = gen rand in
        inst := Some x;
        try
          let res = prop x in
          call x res;
          if not res
            then failures := x :: !failures
        with Prop.PrecondFail ->
          incr precond_failed
      done;
      match !failures with
      | [] -> Ok (n, !precond_failed)
      | _ -> Failed (!failures)
    with e ->
      Error (!inst, e)

  (** {2 Main} *)

  type 'a test_cell = {
    n : int;
    pp : 'a PP.t option;
    prop : 'a Prop.t;
    gen : 'a Arbitrary.t;
    name : string option;
    limit : int;
    size : ('a -> int) option;
  }
  type test =
    | Test : 'a test_cell -> test
    (** GADT needed for the existential type *)

  let name (Test {name; _}) = name

  let display_name {name; _} =
    match name with
    | None -> "<anon prop>"
    | Some n -> n

  let mk_test ?(n=100) ?pp ?name ?size ?(limit=10) gen prop =
    if limit < 0 then failwith "QCheck: limit needs be >= 0";
    if n <= 0 then failwith "QCheck: n needs be >= 0";
    Test { prop; gen; name; n; pp; size; limit; }

  (* tail call version of take, that returns (at most) [n] elements of [l] *)
  let rec _list_take acc l n = match l, n with
    | _, 0
    | [], _ -> List.rev acc
    | x::l', _ -> _list_take (x::acc) l' (n-1)

  let run ?(verbose=false) ?(out=stdout) ?(rand=Random.State.make __seed) (Test test) =
    Printf.fprintf out "testing property %s...\n" (display_name test);
    (* called on each test case *)
    let call x res =
      match test.pp, verbose with
      | Some pp, true ->
        Printf.fprintf out "  test case (%s): %s\n"
          (if res then "ok" else "failed") (pp x)
      | _ -> ()
    in
    match check ~call ~rand ~n:test.n test.gen test.prop with
    | Ok (n, prefail) ->
      Printf.fprintf out "  [✔] passed %d tests (%d preconditions failed)\n" n prefail;
      true
    | Failed l ->
      begin match test.pp with
      | None -> Printf.fprintf out "  [×] %d failures over %d\n" (List.length l) test.n
      | Some pp ->
        Printf.fprintf out "  [×] %d failures over %d (print at most %d):\n"
          (List.length l) test.n test.limit;
        let to_print = match test.size with
        | None -> l
        | Some size ->
          (* sort by increasing size *)
          let l = List.map (fun x -> x, size x) l in
          let l = List.sort (fun (x,sx) (y,sy) -> sx - sy) l in
          List.map fst l
        in
        (* only keep [limit] counter examples *)
        let to_print = _list_take [] to_print test.limit in
        (* print the counter examples *)
        List.iter
          (fun x -> Printf.fprintf out "  %s\n" (pp x))
          to_print
      end;
      false
    | Error (inst, e) ->
      begin match inst, test.pp with
      | _, None
      | None, _ -> Printf.fprintf out "  [×] error: %s\n" (Printexc.to_string e);
      | Some x, Some pp ->
        (* print instance on which the error occurred *)
        Printf.fprintf out "  [×] error on instance %s: %s\n"
          (pp x) (Printexc.to_string e);
      end;
      false

  type suite = test list

  let flatten = List.flatten

  let run_tests ?(verbose=false) ?(out=stdout) ?(rand=Random.State.make __seed) l =
    let start = Unix.gettimeofday () in
    let n = List.length l in
    let failed = ref 0 in
    Printf.fprintf out "check %d properties...\n" (List.length l);
    List.iter
      (fun test ->
        let res = run ~verbose ~out ~rand test in
        flush out;
        if not res then incr failed)
      l;
    Printf.fprintf out "tests run in %.2fs\n" (Unix.gettimeofday() -. start);
    if !failed = 0
      then Printf.fprintf out "[✔] Success! (passed %d tests)\n" n
      else Printf.fprintf out "[×] Failure. (%d tests failed over %d)\n" !failed n;
    !failed = 0

  let run_main ?(argv=Sys.argv) l =
    let verbose = ref false in
    let seed = ref (Random.self_init (); Random.int (1 lsl 29)) in
    let opts = Arg.align
        [ "-v", Arg.Set verbose, " verbose"
        ; "-seed", Arg.Set_int seed, " set random seed"
        ]
    in
    try
      Arg.parse_argv argv opts (fun _ -> invalid_arg "no arguments accepted") "usage: ./tests";
      Printf.printf "use random seed %d\n" !seed;
      let rand = Random.State.make [| !seed |] in
      let ok = run_tests ~verbose:!verbose ~rand l in
      if ok then ()
      else exit 1
    with Arg.Help msg ->
      print_endline msg
end
