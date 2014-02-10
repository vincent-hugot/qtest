(** Adapted from the Jane Street Capital Core quickcheck.ml,
    licensed as LGPL + linking exception *)
(** Module for easily generating unit tests.  Based on code posted by
    padiolea\@irisa.fr to the caml mailing list. *)

open Printf

module RS = Random.State

let rec foldn ~f ~init:acc i =
  if i = 0 then acc else foldn ~f ~init:(f acc i) (i-1)

let sum_int = List.fold_left (+) 0

let (==>) b1 b2 = if b1 then b2 else true (* could use too => *)



(* Value generators *)
type 'a gen = RS.t -> 'a

let ug st = ()

let bg st = RS.bool st

let fg st =
  exp (RS.float st 15. *. (if RS.float st 1. < 0.5 then 1. else -1.))
  *. (if RS.float st 1. < 0.5 then 1. else -1.)

let pfg st = abs_float (fg st)
let nfg st = -.(pfg st)

(* natural number generator *)
let nng st =
  let p = RS.float st 1. in
  if p < 0.5 then RS.int st 10
  else if p < 0.75 then RS.int st 100
  else if p < 0.95 then RS.int st 1_000
  else RS.int st 10_000

let neg_ig st = -(nng st)

(* Uniform random int generator *)
let upos =
  if Sys.word_size = 32 then
    fun st -> RS.bits st
  else (* word size = 64 *)
    fun st ->
      RS.bits st                        (* Bottom 30 bits *)
      lor (RS.bits st lsl 30)           (* Middle 30 bits *)
      lor ((RS.bits st land 3) lsl 60)  (* Top 2 bits *)  (* top bit = 0 *)

let uig st = if RS.bool st then - (upos st) - 1 else upos st

let random_binary_string st length =
  (* 0b011101... *)
  let s = String.create (length + 2) in
  s.[0] <- '0';
  s.[1] <- 'b';
  for i = 0 to length - 1 do
    s.[i+2] <- if RS.bool st then '0' else '1'
  done;
  s

let ui32g st = Int32.of_string (random_binary_string st 32)  
let ui64g st = Int64.of_string (random_binary_string st 64)

let lg_size size gen st =
  foldn ~f:(fun acc _ -> (gen st)::acc) ~init:[] (size st)
let lg gen st = lg_size nng gen st

let ag_size size gen st =
  Array.init (size st) (fun _ -> gen st)
let ag gen st = ag_size nng gen st

let pg gen1 gen2 st = (gen1 st, gen2 st)

let tg g1 g2 g3 st = (g1 st,g2 st, g3 st)


let cg st = char_of_int (RS.int st 255)

let printable_chars =
  let l = 126-32+1 in
  let s = String.create l in
  for i = 0 to l-2 do
    s.[i] <- char_of_int (32+i)
  done;
  s.[l-1] <- '\n';
  s

let printable st = printable_chars.[RS.int st (String.length printable_chars)]
let numeral st = char_of_int (48 + RS.int st 10)

let sg_size ?(gen = cg) size st =
  let s = String.create (size st) in
  for i = 0 to String.length s - 1 do
    s.[i] <- gen st
  done;
  s
let sg ?gen st = sg_size ?gen nng st


(* corner cases *)

let graft_corners gen corners () = 
  let cors = ref corners in fun st ->
    match !cors with [] -> gen st
    | e::l -> cors := l; e

let nng_corners () = graft_corners nng [0;1;2;max_int] ()

(* Additional pretty-printers *)

let pp_list pp l = "[" ^ (String.concat "; " (List.map pp l)) ^ "]"
let pp_array pp l = "[|" ^ (String.concat "; " (Array.to_list (Array.map pp l))) ^ "|]"
let pp_pair p1 p2 (t1,t2) = "(" ^ p1 t1 ^ ", " ^ p2 t2 ^ ")"
let pp_triple p1 p2 p3 (t1,t2,t3) = "(" ^ p1 t1 ^ ", " ^ p2 t2 ^ ", " ^ p3 t3 ^ ")"



(* Generator * pretty-printer pairs *)

type 'a gen_print = 'a gen * ('a -> string)
let unit : unit gen_print = (ug, fun _ -> "()")

let bool = (bg, string_of_bool)

let float = (fg, string_of_float)
let pos_float = (pfg, string_of_float)
let neg_float = (nfg, string_of_float)

let int = (uig, string_of_int)
let pos_int = (upos, string_of_int)
let small_int = (nng, string_of_int)
let small_int_corners () = (nng_corners (), string_of_int)
let neg_int = (neg_ig, string_of_int)
  
let int32 = (ui32g, fun i -> Int32.to_string i ^ "l")
let int64 = (ui64g, fun i -> Int64.to_string i ^ "L")

let char = (cg, sprintf "%C")
let printable_char = (printable, sprintf "%C")
let numeral_char = (numeral, sprintf "%C")

let string_gen_of_size size gen = (sg_size ~gen size, sprintf "%S")
let string_gen gen = (sg ~gen, sprintf "%S")

let string = string_gen cg
let string_of_size size = string_gen_of_size size cg

let printable_string = string_gen printable
let printable_string_of_size size = string_gen_of_size size printable

let numeral_string = string_gen numeral
let numeral_string_of_size size = string_gen_of_size size numeral

let list (gen,pp) = (lg gen, pp_list pp)
let list_of_size size (gen,pp) = (lg_size size gen, pp_list pp)

let array (gen,pp) = (ag gen, pp_array pp)
let array_of_size size (gen,pp) = (ag_size size gen, pp_array pp)

let pair (g1,p1) (g2,p2) = (pg g1 g2, pp_pair p1 p2)
let triple (g1,p1) (g2,p2) (g3,p3) = (tg g1 g2 g3, pp_triple p1 p2 p3)

let option (g1, p1) =
  let g st =
    let p = RS.float st 1. in
    if p < 0.15 then None
    else Some (g1 st) in
  let p = function
    | None -> "None"
    | Some x -> "Some " ^ p1 x in
  (g, p)

let fun1 : 'a gen_print -> 'b gen_print -> ('a -> 'b) gen_print =
  fun (_g1, p1) (g2, p2) ->
    let magic_object = Obj.magic (object end) in
    let gen : ('a -> 'b) gen = fun st ->
      let h = Hashtbl.create 10 in
      fun x ->
        if x == magic_object then
          Obj.magic h
        else
          try Hashtbl.find h x
          with Not_found ->
            let b = g2 st in
            Hashtbl.add h x b;
            b in
    let pp : ('a -> 'b) -> string = fun f ->
      let h : ('a, 'b) Hashtbl.t = Obj.magic (f magic_object) in
      let b = Buffer.create 20 in
      Hashtbl.iter (fun key value -> Printf.bprintf b "%s -> %s; " (p1 key) (p2 value)) h;
      "{" ^ Buffer.contents b ^ "}" in
    gen, pp

let fun2 gp1 gp2 gp3 = fun1 gp1 (fun1 gp2 gp3)

(* Generator combinators *)


(** given a list, returns generator that picks at random from list *)
let oneofl xs () =
  List.nth xs (Random.int (List.length xs))

(** Given a list of generators, returns generator that randomly uses one of the generators
    from the list *)
let oneof xs =
  List.nth xs (Random.int (List.length xs))

(** Generator that always returns given value *)
let always x () = x

(** Given list of [(frequency,value)] pairs, returns value with probability proportional
    to given frequency *)
let frequency xs =
  let sums = sum_int (List.map fst xs) in
  let i = Random.int sums in
  let rec aux acc = function
    | ((x,g)::xs) -> if i < acc+x then g else aux (acc+x) xs
    | _ -> failwith "frequency"
  in
  aux 0 xs

(** like frequency, but returns generator *)
let frequencyl l = frequency (List.map (fun (i,e) -> (i,always e)) l)



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
let rec laws_smallest measure iter gen func st =
  let return = ref None in
  let register input =
    match !return with
    | None ->
      return := Some input
    | Some x ->
      if measure input < measure x then
      return := Some input
  in
  for i = 1 to iter do
    let input = gen st in
    try if not (func input) then register input
    with _ -> register input
  done;
  !return


let default_count = 100

(** Like laws, but throws an exception instead of returning an option.  *)
let laws_exn ?small ?(count=default_count) name (gen,pp) func st =
  let result = match small with
  | None -> laws count gen func st
  | Some measure -> laws_smallest measure count gen func st
  in match result with
    | None -> ()
    | Some i -> failwith (Printf.sprintf "law %s failed for %s" name (pp i))

let rec statistic_number = function
  | []    -> []
  | x::xs -> let (splitg, splitd) = List.partition (fun y -> y = x) xs in
    (1 + List.length splitg, x) :: statistic_number splitd

(* in percentage *)
let statistic xs =
  let stat_num = statistic_number xs in
  let totals = sum_int (List.map fst stat_num) in
  List.map (fun (i, v) -> ((i * 100) / totals), v) stat_num

let laws2 iter func gen =
  let res = foldn ~init:[] iter
    ~f:(fun acc _ -> let n = gen () in (n, func n) :: acc)
  in
  let stat = statistic (List.map (fun (_, (_, v)) -> v) res) in
  let res = List.filter (fun (_, (b, _)) -> not b) res in
  if res = [] then (None, stat) else (Some (fst (List.hd res)), stat)
