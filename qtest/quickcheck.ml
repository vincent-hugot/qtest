(** Adapted from the Jane Street Capital Core quickcheck.ml,
    licensed as LGPL + linking exception *)
(** Module for easily generating unit tests.  Based on code posted by
    padiolea\@irisa.fr to the caml mailing list. *)

open Printf

module RS = Random.State

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let _opt_or ~d ~f = function
  | None -> d
  | Some x -> f x

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

let (==>) b1 b2 = if b1 then b2 else true (* could use too => *)

module Gen = struct
  type 'a t = RS.t -> 'a
  type 'a sized = int -> Random.State.t -> 'a

  let return x _st = x

  let (>>=) gen f st =
    f (gen st) st

  let (<*>) f x st = f st (x st)
  let lift f x st = f (x st)
  let lift2 f x y st = f (x st) (y st)
  let lift3 f x y z st = f (x st) (y st) (z st)

  let oneof l st = List.nth l (Random.State.int st (List.length l)) st
  let oneofl xs st = List.nth xs (Random.State.int st (List.length xs))

  let frequencyl l st =
    let sums = sum_int (List.map fst l) in
    let i = Random.State.int st sums in
    let rec aux acc = function
      | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs
      | _ -> failwith "frequency"
    in
    aux 0 l

  let frequency l st = frequencyl l st st

  (* natural number generator *)
  let nat st =
    let p = RS.float st 1. in
    if p < 0.5 then RS.int st 10
    else if p < 0.75 then RS.int st 100
    else if p < 0.95 then RS.int st 1_000
    else RS.int st 10_000

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

(* Additional pretty-printers *)

let pp_list pp l = "[" ^ (String.concat "; " (List.map pp l)) ^ "]"
let pp_array pp l = "[|" ^ (String.concat "; " (Array.to_list (Array.map pp l))) ^ "|]"
let pp_pair p1 p2 (t1,t2) = "(" ^ p1 t1 ^ ", " ^ p2 t2 ^ ")"
let pp_triple p1 p2 p3 (t1,t2,t3) = "(" ^ p1 t1 ^ ", " ^ p2 t2 ^ ", " ^ p3 t3 ^ ")"



(* arbitrary instances *)

type 'a arbitrary = {
  gen: 'a Gen.t;
  print: ('a -> string) option; (** print values *)
  small: ('a -> int) option;  (** size of example *)
  shrink: ('a -> 'a list) option;  (** shrink to smaller examples *)
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
let shrink_nil _ = []

let make_scalar ?print ?collect gen =
  make ~shrink:shrink_nil ~small:small1 ?print ?collect gen

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
  make ~small:small1 ~shrink:shrink_nil ~print:(fun _ -> "()") Gen.unit

let bool = make_scalar ~print:string_of_bool Gen.bool
let float = make_scalar ~print:string_of_float Gen.float
let pos_float = make_scalar ~print:string_of_float Gen.pfloat
let neg_float = make_scalar ~print:string_of_float Gen.nfloat

let int = make_scalar ~print:string_of_int Gen.int
let pos_int = make_scalar ~print:string_of_int Gen.pint
let small_int = make_scalar ~print:string_of_int Gen.nat
let small_int_corners () = make_scalar ~print:string_of_int (Gen.nng_corners ())
let neg_int = make_scalar ~print:string_of_int Gen.neg_int

let int32 = make_scalar ~print:(fun i -> Int32.to_string i ^ "l") Gen.ui32
let int64 = make_scalar ~print:(fun i -> Int64.to_string i ^ "L") Gen.ui64

let char = make_scalar ~print:(sprintf "%C") Gen.char
let printable_char = make_scalar ~print:(sprintf "%C") Gen.printable
let numeral_char = make_scalar ~print:(sprintf "%C") Gen.numeral

let string_gen_of_size size gen =
  make ~small:String.length ~print:(sprintf "%S") (Gen.string_size ~gen size)
let string_gen gen =
  make ~small:String.length ~print:(sprintf "%S") (Gen.string ~gen)

let string = string_gen Gen.char
let string_of_size size = string_gen_of_size size Gen.char

let printable_string = string_gen Gen.printable
let printable_string_of_size size = string_gen_of_size size Gen.printable

let numeral_string = string_gen Gen.numeral
let numeral_string_of_size size = string_gen_of_size size Gen.numeral

let shrink_list_ l =
  let rec remove_one l r = match r with
    | [] -> []
    | x :: tail -> (List.rev_append l r) :: remove_one (x :: l) tail
  in
  remove_one [] l

let list_sum_ f l = List.fold_left (fun acc x-> f x+acc) 0 l

let list a =
  (* small sums sub-sizes if present, otherwise just length *)
  let small = _opt_or a.small ~f:list_sum_ ~d:List.length in
  let print = _opt_map a.print ~f:pp_list in
  make
    ~small
    ~shrink:shrink_list_
    ?print
    (Gen.list a.gen)

let list_of_size size a =
  let small = _opt_or a.small ~f:list_sum_ ~d:List.length in
  let print = _opt_map a.print ~f:pp_list in
  make
    ~small
    ~shrink:shrink_list_
    ?print
    (Gen.list_size size a.gen)

let array_sum_ f a = Array.fold_left (fun acc x -> f x+acc) 0 a

let shrink_array_ a =
  let b = Array.init (Array.length a)
    (fun i ->
      Array.init (Array.length a-1)
        (fun j -> if j<i then a.(j) else a.(j-1))
    ) in
  Array.to_list b

let array a =
  let small = _opt_or ~d:Array.length ~f:array_sum_ a.small in
  make
    ~small
    ~shrink:shrink_array_
    ?print:(_opt_map ~f:pp_array a.print)
    (Gen.array a.gen)

let array_of_size size a =
  let small = _opt_or ~d:Array.length ~f:array_sum_ a.small in
  make
    ~small
    ~shrink:shrink_array_
    ?print:(_opt_map ~f:pp_array a.print)
    (Gen.array_size size a.gen)

(* TODO: add shrinking *)

let pair a b =
  make
    ?small:(_opt_map_2 ~f:(fun f g (x,y) -> f x+g y) a.small b.small)
    ?print:(_opt_map_2 ~f:pp_pair a.print b.print)
    (Gen.pair a.gen b.gen)

let triple a b c =
  make
    ?small:(_opt_map_3 ~f:(fun f g h (x,y,z) -> f x+g y+h z) a.small b.small c.small)
    ?print:(_opt_map_3 ~f:pp_triple a.print b.print c.print)
    (Gen.triple a.gen b.gen c.gen)

let option a =
  let some_ x = Some x in
  let g f st =
    let p = RS.float st 1. in
    if p < 0.15 then None
    else Some (f st)
  and p f = function
    | None -> "None"
    | Some x -> "Some " ^ f x
  and small =
    _opt_or a.small ~d:(function None -> 0 | Some _ -> 1)
      ~f:(fun f o -> match o with None -> 0 | Some x -> f x)
  and shrink =
    _opt_map a.shrink
    ~f:(fun f o -> match o with None -> [] | Some x -> List.map some_ (f x))
  in
  make
    ~small
    ?shrink
    ?print:(_opt_map ~f:p a.print)
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
let oneofl ?print ?collect xs =
  make ?print ?collect (Gen.oneofl xs)

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
let frequencyl ?print ?small l =
  make ?print ?small (Gen.frequencyl l)

let map ?rev f a =
  make
    ?print:(_opt_map_2 rev a.print ~f:(fun r p x -> p (r x)))
    ?small:(_opt_map_2 rev a.small ~f:(fun r s x -> s (r x)))
    ?shrink:(_opt_map_2 rev a.shrink ~f:(fun r g x -> List.map f @@ g (r x)))
    ?collect:(_opt_map_2 rev a.collect ~f:(fun r f x -> f (r x)))
    (fun st -> f (a.gen st))

let map_same_type f a =
  adapt_ a (fun st -> f (a.gen st))

(* Laws *)


(** [laws iter gen func] applies [func] repeatedly ([iter] times) on output of [gen], and
    if [func] ever returns false, then the input that caused the failure is returned
    optionally.  *)
let rec laws iter gen func st =
  if iter <= 0 then None
  else
    let input = gen st in
    try
      if not (func input) then Some input
      else laws (iter-1) gen func st
    with _ -> Some input

(** like [laws], but executes all tests anyway and returns optionally the
  smallest failure-causing input, wrt. some measure *)
let laws_smallest measure iter gen func st =
  let return = ref None in
  let register input =
    match !return with
    | None ->
      return := Some input
    | Some x ->
      if measure input < measure x then
      return := Some input
  in
  for _i = 1 to iter do
    let input = gen st in
    try if not (func input) then register input
    with _ -> register input
  done;
  !return

(* TODO sized generators (use them everywhere but allow make to
   take generator that ignores size) *)

(* TODO: redefine [==>]; if assumption failed, try to generate new one
   but set limit to, say, 1000 *)

(* TODO: shrinking if available; otherwise use smallest if small defined *)

let default_count = 100

exception LawFailed of string

let no_print_ _ = "<no printer>"

(** Like laws, but throws an exception instead of returning an option.  *)
let laws_exn ?(count=default_count) name a func st =
  let result = match a.small with
  | None -> laws count a.gen func st
  | Some measure -> laws_smallest measure count a.gen func st
  in match result with
    | None -> ()
    | Some i ->
        let pp = match a.print with None -> no_print_ | Some x -> x in
        let msg = Printf.sprintf "law %s failed for %s" name (pp i) in
        raise (LawFailed msg)

let rec statistic_number = function
  | []    -> []
  | x::xs -> let (splitg, splitd) = List.partition (fun y -> y = x) xs in
    (1 + List.length splitg, x) :: statistic_number splitd

(* in percentage *)
let statistic xs =
  let stat_num = statistic_number xs in
  let totals = sum_int (List.map fst stat_num) in
  List.map (fun (i, v) -> ((i * 100) / totals), v) stat_num

(* TODO: expose in .mli? document *)
let laws2 iter func gen =
  let res = foldn ~init:[] iter
    ~f:(fun acc _ -> let n = gen () in (n, func n) :: acc)
  in
  let stat = statistic (List.map (fun (_, (_, v)) -> v) res) in
  let res = List.filter (fun (_, (b, _)) -> not b) res in
  if res = [] then (None, stat) else (Some (fst (List.hd res)), stat)
