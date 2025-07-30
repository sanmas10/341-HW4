open Hw4types

(* 1 *)
let rec sequence spacing low high : int list =
  let rec go current =
    if current > high then
      []
    else
      current :: go (current + spacing)
  in
  go low

(* 2 *)
let string_append_map xs suffix =
  List.map (fun s -> s ^ suffix) xs

(* 3 *)
let list_nth_mod xs n =
  (* First, guard against negative inputs as required. *)
  if n < 0 then
    raise Negative_Input
  else
    match xs with
    | [] -> raise EmptyList
    | _ ->
        (* Get its length. *)
        let len = List.length xs in
        (* Calculate the index using the modulo operator. *)
        let index = n mod len in
        (* Return the element at the calculated index. *)
        List.nth xs index

(* 4 *)
let rec stream_first_k_such_that p k (Stream t) =
  if k = 0 then
    []
  else
    match t () with
    (* If the stream is empty, we can't get any more elements. *)
    | None ->
        (* As k is not 0 here, we haven't found enough elements, so raise an exception. *)
        raise EmptyStream
    (* If the stream has a value 'v' and a 'next_s' stream. *)
    | Some (v, next_s) ->
        (* Check if the value 'v' satisfies the predicate 'p'. *)
        if p v then
          (* If it does, add 'v' to our result list and recursively find the remaining k-1 elements. *)
          v :: stream_first_k_such_that p (k - 1) next_s
        else
          (* If not, discard 'v' and recursively look for k elements in the rest of the stream. *)
          stream_first_k_such_that p k next_s

(* 5 *)
let funny_number_stream : int stream =
  (* Define a recursive helper function 'go' that takes the current number 'n'
   * and generates the stream from that point onward. *)
  let rec go n =
    Stream
      (fun () ->
        (* Determine the value: if n is divisible by 6, negate it; otherwise, use n. *)
        let v = if n mod 6 = 0 then -n else n in
        (* The stream produces the value 'v' and a thunk for the next part of the stream,
         * which is a recursive call to 'go' with n+1. *)
        Some (v, go (n + 1)))
  in
  (* The stream starts by calling our generator with the number 1. *)
  go 1

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
