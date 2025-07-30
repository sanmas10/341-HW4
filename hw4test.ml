open Hw4lib
open Hw4

(* 1 *)
let%test _ = sequence 2 3 11 = [3; 5; 7; 9; 11]
let%test _ = sequence 3 3 8 = [3; 6]
let%test _ = sequence 1 3 2 = []

(* 2 *)
let%test _ = string_append_map [] "foo" = []
(* more tests here *)



