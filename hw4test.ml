open Hw4lib
open Hw4
open Hw4types

(* Problem 1 *)
let%test _ = sequence 2 3 11 = [3; 5; 7; 9; 11]
let%test _ = sequence 3 3 8 = [3; 6]
let%test _ = sequence 1 3 2 = []
let%test _ = sequence 10 5 5 = [5] (* Low and high are the same. *)
let%test _ = sequence 10 0 9 = [0] (* Next number (0 + 10) would exceed high, so the list should only contain the starting number. *)

(* Problem 2 *)
let%test _ = string_append_map [] "foo" = []
let%test _ = string_append_map ["a"; "b"; "c"] "!" = ["a!"; "b!"; "c!"]
let%test _ = string_append_map ["hello"] " world" = ["hello world"]
let%test _ = string_append_map ["ocaml"; "is"; "fun"] "" = ["ocaml"; "is"; "fun"]
let%test _ = string_append_map [""; ""] "test" = ["test"; "test"]

(* Problem 3 *)
let%test _ = list_nth_mod [10; 20; 30] 1 = 20
let%test _ = list_nth_mod ["a"; "b"] 3 = "b" (* 3 mod 2 = 1, which is "b" *)
let%test _ = list_nth_mod [5] 100 = 5 (* 100 mod 1 = 0, which is 5 *)

let%test _ =
  try
    let _ = list_nth_mod [] 5 in
    false (* Fail if EmptyList is not raised *)
  with
  | EmptyList -> true (* Pass if EmptyList is raised *)

let%test _ =
  try
    let _ = list_nth_mod [1; 2] (-1) in
    false (* Fail if Negative_Input is not raised *)
  with
  | NegativeInput -> true (* Pass if Negative_Input is raised *)

(* Problem 4 *)
(* Helper function to create a finite stream from a list for testing. *)
let rec stream_of_list xs =
  let r = ref xs in
  Stream
    (fun () ->
      match !r with
      | [] -> None
      | h :: t ->
          r := t;
          Some (h, stream_of_list !r))

(* A simple infinite stream of natural numbers for testing. *)
let rec from n = Stream (fun () -> Some (n, from (n + 1)))

let%test _ =
  let is_even n = n mod 2 = 0 in
  stream_first_k_such_that is_even 3 (from 1) = [2; 4; 6]

let%test _ =
  stream_first_k_such_that (fun _ -> true) 0 (from 1) = []

let%test _ =
  let s = stream_of_list [1; 2; 3; 4; 5] in
  stream_first_k_such_that (fun x -> x > 0) 5 s = [1; 2; 3; 4; 5]

let%test _ =
  try
    let s = stream_of_list [2; 4] in
    let _ = stream_first_k_such_that (fun x -> x mod 2 = 0) 3 s in
    false (* Fail if EmptyStream is not raised *)
  with
  | EmptyStream -> true (* Pass if EmptyStream is raised *)


(* Problem 5 *)
let%test _ =
  stream_first_k_such_that (fun _ -> true) 7 funny_number_stream
  = [1; 2; 3; 4; 5; -6; 7]

let%test _ =
  stream_first_k_such_that (fun _ -> true) 13 funny_number_stream
  = [1; 2; 3; 4; 5; -6; 7; 8; 9; 10; 11; -12; 13]


(* Problem 6 *)
let%test _ =
  stream_first_k_such_that (fun _ -> true) 1 foo_then_bar = ["foo"]

let%test _ =
  stream_first_k_such_that (fun _ -> true) 4 foo_then_bar = ["foo"; "bar"; "foo"; "bar"]

let%test _ =
  stream_first_k_such_that (fun _ -> true) 5 foo_then_bar = ["foo"; "bar"; "foo"; "bar"; "foo"]

(* Problem 7 *)
let%test _ =
  let s = stream_of_list ["a"; "b"; "c"] in
  let p = stream_pair_all_with_one s in
  stream_first_k_such_that (fun _ -> true) 3 p = [(1, "a"); (1, "b"); (1, "c")]

let%test _ =
  let s = from 5 in (* Helper from Problem 4 tests: 5, 6, 7, ... *)
  let p = stream_pair_all_with_one s in
  stream_first_k_such_that (fun _ -> true) 4 p = [(1, 5); (1, 6); (1, 7); (1, 8)]

let%test _ =
  let s = stream_of_list [] in
  let p = stream_pair_all_with_one s in
  stream_first_k_such_that (fun _ -> true) 0 p = []

(* Problem 8 *)
let%test _ =
  let xs = [1; 2; 3] in
  let ys = ["a"; "b"] in
  let s = cycle_lists xs ys in
  let expected =
    [(1, "a"); (2, "b"); (3, "a"); (1, "b"); (2, "a"); (3, "b"); (1, "a")]
  in
  stream_first_k_such_that (fun _ -> true) 7 s = expected

let%test _ =
  let xs = ["foo"] in
  let ys = [10; 20; 30; 40] in
  let s = cycle_lists xs ys in
  let expected = [("foo", 10); ("foo", 20); ("foo", 30); ("foo", 40); ("foo", 10)] in
  stream_first_k_such_that (fun _ -> true) 5 s = expected

(* Problem 9 *)
let%test _ =
  let s = from 1 in (* 1, 2, 3, 4, 5, ... *)
  let advanced_s = advance_until 3 s in
  (* The advanced stream should start from 4. *)
  stream_first_k_such_that (fun _ -> true) 3 advanced_s = [4; 5; 6]

let%test _ =
  let s = from 10 in (* 10, 11, 12, ... *)
  let advanced_s = advance_until 10 s in
  (* The advanced stream should start from 11. *)
  stream_first_k_such_that (fun _ -> true) 2 advanced_s = [11; 12]

let%test _ =
  try
    let s = stream_of_list [1; 2; 3] in
    let _ = advance_until 5 s in
    false (* Fail if EmptyStream is not raised *)
  with
  | EmptyStream -> true (* Pass if EmptyStream is raised *)

(* Problem 10 *)
let%test _ =
  let a = [|Some ("a", 1); Some ("b", 2); Some ("c", 3)|] in
  array_assoc "b" a = Some 2

let%test _ =
  let a = [|Some ("a", 1); None; Some ("c", 3)|] in
  array_assoc "c" a = Some 3

let%test _ =
  let a = [|Some ("a", 1); Some ("b", 2)|] in
  array_assoc "d" a = None

let%test _ =
  let a = [||] in (* An empty array literal *)
  array_assoc "a" a = None

let%test _ =
  let a = [|Some ("x", 10); Some ("y", 20); Some ("x", 30)|] in
  (* Should return the value from the first match. *)
  array_assoc "x" a = Some 10

(* Problem 11 *)
let%test _ =
  let xs = [("a", 1); ("b", 2); ("c", 3); ("d", 4); ("e", 5)] in
  (* Create a cached lookup function with a cache of size 3. *)
  let f = caching_assoc xs 3 in
  (* First lookup for "c" -> Miss, finds it in xs, adds to cache[0], returns Some 3. *)
  f "c" = Some 3
  (* Second lookup for "c" -> Hit, finds it in cache, returns Some 3. *)
  && f "c" = Some 3
  (* Lookup for "a" -> Miss, adds to cache[1]. *)
  && f "a" = Some 1
  (* Lookup for "e" -> Miss, adds to cache[2]. Cache is now full. *)
  && f "e" = Some 5
  (* Lookup for "d" -> Miss, adds to cache[0] (replaces "c"). *)
  && f "d" = Some 4
  (* Lookup for key not in xs. *)
  && f "z" = None
  (* Lookup for "c" again. It was replaced in the cache, so this is a miss,
     but it should still find it in the original list. *)
  && f "c" = Some 3

(* Problem 12 *)
let%test _ =
  tokenize "10 5 +" = [Literal 10; Literal 5; Plus]

let%test _ =
  tokenize "1 2 - 3 * ." = [Literal 1; Literal 2; Minus; Literal 3; Mul; Dot]

let%test _ =
  tokenize "  10   20  " = [Literal 10; Literal 20] (* Handles extra spaces *)

let%test _ =
  tokenize "" = [] (* Handles empty input string *)

let%test _ =
  try
    let _ = tokenize "10 5 abc +" in
    false (* Fail if TrefoilError is not raised *)
  with
  | TrefoilError "Invalid token: abc" -> true (* Pass if the correct error is raised *)

(* Problem 13 *)
let%test _ =
  interpret [] (tokenize "5 10 +") = [15]

let%test _ =
  interpret [100] (tokenize "5 + 3 *") = [315]

let%test _ =
  interpret [] (tokenize "10 4 -") = [6]

let%test _ =
  (* Trace: initial stack [] -> "1 2 3" -> [3; 2; 1] -> "*" -> [6; 1] -> "-" -> [-5] *)
  interpret [] (tokenize "1 2 3 * -") = [-5]

let%test _ =
  interpret [10] (tokenize "") = [10]

let%test _ =
  try
    let _ = interpret [5] (tokenize "+") in
    false
  with
  | TrefoilError "Stack underflow" -> true

let%test _ =
  try
    let _ = interpret [] (tokenize ".") in
    false
  with
  | TrefoilError "Stack underflow" -> true