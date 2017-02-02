(* first in the original file *)
let _id1 x = x;;
(*$T _id1
  _id1 true
  _id1 (2 = 2)
  _id1 (3 + 1 = 4)
*)

# 8 "another_file.ml"
(* then in another file *)
let _id2 x = x;;
(*$T _id2
  _id2 true
*)

# 3 "with_different_lines.ml"
let _id3 x = x;;
(*$T _id3
  _id3 true
  _id3 (5 + 1 = 6)
*)
# 20 "final_file.ml"
(* in the middle of things *)
let _id4 x = x;;
(*$T _id4
  _id4 true
  _id4 (1 = 1)
  _id4 true
  _id4 true
  _id4 true
*)
