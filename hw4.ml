open Hw4types

(* Make sure that running both dune build and dune test work without error. Do not modify hw4types.ml or hw4.mli and do not turn them in. *)
 
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
    raise NegativeInput
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
    (* If the stream has a value v and a next_s stream. *)
    | Some (v, next_s) ->
        (* Check if the value v satisfies the predicate p . *)
        if p v then
          (* If it does, add v to our result list and recursively find the remaining k-1 elements. *)
          v :: stream_first_k_such_that p (k - 1) next_s
        else
          (* If not, discard v and recursively look for k elements in the rest of the stream. *)
          stream_first_k_such_that p k next_s

(* 5 *)
let funny_number_stream : int stream =
  (* A recursive helper generates the stream, starting from number n. *)
  let rec go n =
    Stream
      (fun () ->
        (* If n is divisible by 6, negate it. Otherwise, use n as the value. *)
        let v = if n mod 6 = 0 then -n else n in
        (* Produce the value v and the rest of the stream, which starts from n+1. *)
        Some (v, go (n + 1)))
  in
  (* Start the stream from 1. *)
  go 1

(* 6 *)
let foo_then_bar : string stream =
  (* To create an alternating stream, we can use two mutually recursive values. *)
  let rec go_foo =
    Stream (fun () ->
      (* This stream state produces foo and sets the next state to go_bar. *)
      Some ("foo", go_bar))
  and go_bar =
    Stream (fun () ->
      (* This stream state produces bar and alternates back to the go_foo state. *)
      Some ("bar", go_foo))
  in
  (* The final stream starts in the go_foo state to produce foo first. *)
  go_foo

(* 7 *)
let rec stream_pair_all_with_one (Stream t) =
  Stream (fun () ->
    (* Evaluate the input stream s by calling its thunk t. *)
    match t () with
    (* If the input stream is empty (returns None), this new stream is also empty. *)
    | None -> None
    (* If the input stream produces a value v and a tail next_s. *)
    | Some (v, next_s) ->
        (* The new stream produces the pair (1, v) and its tail is a recursive
         * call on the tail of the input stream. *)
        Some ((1, v), stream_pair_all_with_one next_s)
  )

(* 8 *)
let cycle_lists xs ys =
  (* Helper that builds the stream using n to track position *)
  let rec go n =
    Stream (fun () ->
      (* Use n mod length to loop through both lists *)
      let x = list_nth_mod xs n in
      let y = list_nth_mod ys n in
      (* Output (x, y) and keep going with n + 1 *)
      Some ((x, y), go (n + 1))
    )
  in
  (* Start the stream generation from the first position, n=0. *)
  go 0

(* 9 *)
let rec advance_until x (Stream t) =
  (* Evaluate the streams thunk t to see the next element. *)
  match t () with
  (* If the stream ends before we find x, we must raise an exception. *)
  | None -> raise EmptyStream
  (* If the stream produces a value v and the rest of the stream next_s. *)
  | Some (v, next_s) ->
      (* Check if the current value v is the target value x. *)
      if v = x then
        (* If we found it, return the rest of the stream. *)
        next_s
      else
        (* If not, keep searching in the rest of the stream. *)
        advance_until x next_s

(* 10 *)
let array_assoc key a =
  let len = Array.length a in
  (* Helper function that checks each index starting from i *)
  let rec go i =
    (* If the index i is past the end of the array,
     * it means we've searched the whole thing and found no match. *)
    if i >= len then
      None
    else
      (* Check what's at index i in the array *)
      match a.(i) with
      (* If the slot is empty (None), we skip it and check the next index. *)
      | None -> go (i + 1)
      (* If the slot contains a key-value pair (k', v). *)
      | Some (k', v) ->
          (* Check if the key k' matches the one we're searching for. *)
          if k' = key then
            (* If it's a match, we're done. Return Some v. *)
            Some v
          else
            (* If it's not a match, continue searching at the next index. *)
            go (i + 1)
  in
  (* Start the search from the beginning of the array at index 0. *)
  go 0

(* 11 *)
let caching_assoc xs n =
  (* Make the cache and a counter for which slot to use next *)
  let cache = Array.make n None in
  let next_cache_slot = ref 0 in

  fun k ->
    match array_assoc k cache with
    | Some v ->
        (* The value was found in the cache so we eturn it immediately. *)
        Some v
    | None ->
        (* The value was not in the cache so we search the original list. *)
        match List.assoc_opt k xs with
        | None ->
            (* The key is not in the original list so the result is None. *)
            None
        | Some v ->
            (* The key exists so must add the (key, value) pair
             * to our cache before returning the value. *)
            let current_slot = !next_cache_slot in
            
            (* Store the new pair in the cache at the current round-robin position. *)
            cache.(current_slot) <- Some (k, v);
            
            (* Update the counter for the next write, wrapping around with modulo. *)
            next_cache_slot := (current_slot + 1) mod n;
            
            (* Return the value we found. *)
            Some v

(* 12 *)
let tokenize s =
  (* We split the input string by spaces to get a list of parts.  *)
  let parts = String.split_on_char ' ' s in
  (* Filter out empty strings that can result from multiple spaces in a row. *)
  let filtered_parts = List.filter (fun p -> p <> "") parts in

  (* A recursive helper function to convert the list of string parts into a token list. *)
  let rec go ps =
    match ps with
    | [] -> [] (* The base, if there are no more parts, we're done. *)
    | p :: ps_tail -> (* Process the head p and recurse on the tail. *)
        (* Match the string part to its corresponding token type. *)
        match p with
        | "+" -> Plus :: go ps_tail
        | "-" -> Minus :: go ps_tail
        | "*" -> Mul :: go ps_tail
        | "." -> Dot :: go ps_tail
        | num_str ->
            (* If it's not a known operator, try to convert it to an integer literal. *)
            match int_of_string_opt num_str with
            | Some i -> (Literal i) :: go ps_tail
            | None ->
                (* If it's not an operator or an integer, it's a ivalid token.  *)
                raise (TrefoilError ("Invalid token: " ^ num_str))
  in
  go filtered_parts

(* 13 *)
let rec interpret stack ts =
  match ts with
  | [] -> stack
  | t :: ts_tail -> (
      match t, stack with
      | Literal n, _ -> interpret (n :: stack) ts_tail

      (* Binary operators require at least two values on the stack. *)
      | Plus, v1 :: v2 :: s_tail -> interpret ((v2 + v1) :: s_tail) ts_tail
      | Mul, v1 :: v2 :: s_tail -> interpret ((v2 * v1) :: s_tail) ts_tail
      | Minus, v1 :: v2 :: s_tail -> interpret ((v2 - v1) :: s_tail) ts_tail

      (* Dot requires one value. *)
      | Dot, v :: s_tail ->
          print_endline (string_of_int v);
          interpret s_tail ts_tail

      (* Any other case for these tokens is a stack underflow. *)
      | (Plus | Mul | Minus | Dot), _ -> raise (TrefoilError "Stack underflow"))
