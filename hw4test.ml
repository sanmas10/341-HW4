open Hw4lib
open Hw4

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
  | Negative_Input -> true (* Pass if Negative_Input is raised *)

(* Problem 4 *)

(* Helper function to create a finite stream from a list for testing. *)
let stream_of_list xs =
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