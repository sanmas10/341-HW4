open Hw4types

(* 1 *)
let rec sequence spacing low high : int list =
  failwith "sequence: not implemented"

(* 2 *)
let string_append_map xs suffix = failwith "string_append_map: not implemented"

(* 3 *)
let list_nth_mod xs n = failwith "list_nth_mod: not implemented"

(* 4 *)
let rec stream_first_k_such_that p k (Stream t) =
  failwith "stream_first_k_such_that: not implemented"

(* 5 *)
let funny_number_stream : int stream =
  Stream (fun () -> failwith "funny_number_stream: not implemented")

(* 6 *)
let foo_then_bar : string stream =
  Stream (fun () -> failwith "foo_then_bar: not implemented")

(* 7 *)
let stream_pair_all_with_one s =
  failwith "stream_pair_all_with_one: not implemented"

(* 8 *)
let cycle_lists xs ys = failwith "cycle_lists: not implemented"

(* 9 *)
let rec advance_until x s = failwith "advance_until: not implemented"

(* 10 *)
let array_assoc key a = failwith "array_assoc: not implemented"

(* 11 *)
let caching_assoc xs n = failwith "caching_assoc: not implemented"

(* 12 *)
let tokenize s = failwith "tokenize: not implemented"

(* 13 *)
let rec interpret stack ts = failwith "interpret: not implemented"
