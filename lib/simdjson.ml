(*---------------------------------------------------------------------------
   Copyright (c) 2021 The ocaml-simdjson programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type p
type e
type o
type oi
type a
type ai
type ccds
type dsi
type value = {e: e; gen: unit ref}
type t = {p: p; mutable freed: bool; gen: unit ref}
type ds = {ds: ccds; gen: unit ref; buf: Bigstringaf.t option}

external threadsEnabled : unit -> bool = "threadsEnabled" [@@noalloc]

let threadsEnabled = lazy (threadsEnabled ())

external elementType : e -> char = "elementType_stubs" [@@noalloc]
external createParser : unit -> p = "createParser_stubs"
external freeParser : p -> unit = "freeParser_stubs" [@@noalloc]
external parseString : p -> string -> int -> e = "parseString_stubs"
external parse : p -> Bigstringaf.t -> int -> e = "parse_stubs"

external parseMany : p -> Bigstringaf.t -> int -> int -> ccds
  = "parseMany_stubs"

external load : p -> string -> e = "load_stubs"
external loadMany : p -> string -> int -> ccds = "loadMany_stubs"
external freeDs : ccds -> unit = "freeDs_stubs" [@@noalloc]

(* *)
external getBool : e -> bool = "getBool_stubs" [@@noalloc]
external getInt64 : e -> int64 = "getInt64_stubs"
external getUint64 : e -> int64 = "getUint64_stubs"
external getDouble : e -> float = "getDouble_stubs"
external getString : e -> string = "getString_stubs"
external getObject : e -> o = "getObject_stubs"
external getArray : e -> a = "getArray_stubs"

(* *)
external docStreamIteratorBegin : ccds -> dsi = "docStreamIteratorBegin_stubs"
external docStreamIteratorEnd : ccds -> dsi = "docStreamIteratorEnd_stubs"

external docStreamIteratorCompare : dsi -> dsi -> bool
  = "docStreamIteratorCompare_stubs"
  [@@noalloc]

external docStreamIteratorGet : dsi -> e = "docStreamIteratorGet_stubs"

external docStreamIteratorNext : dsi -> unit = "docStreamIteratorNext_stubs"
  [@@noalloc]

external docStreamIteratorIndex : dsi -> int = "currentIndex_stubs" [@@noalloc]

(* *)
external arraySize : a -> int = "arraySize_stubs" [@@noalloc]
external arrayIterator : a -> ai = "arrayIterator_stubs"
external arrayIteratorGet : ai -> e = "arrayIteratorGet_stubs"
external arrayIteratorNext : ai -> unit = "arrayIteratorNext_stubs" [@@noalloc]

(* *)
external objSize : o -> int = "objSize_stubs" [@@noalloc]
external objIterator : o -> oi = "objIterator_stubs"
external objIteratorGet : oi -> string * e = "objIteratorGet_stubs"
external objIteratorNext : oi -> unit = "objIteratorNext_stubs" [@@noalloc]

let raise_if_already_freed freed msg =
  if freed then Printf.ksprintf failwith "%s: parser used after being free" msg

let raise_if_different_gen freed msg =
  if freed then Printf.ksprintf failwith "%s: parser is outdated" msg

let create () = {p= createParser (); freed= false; gen= ref ()}
let free {p; freed; _} = if freed then () else freeParser p
let free_ds {ds; _} = freeDs ds
let paddingLen = 32
let padding = Bigstringaf.create paddingLen
let defaultBatchSize = 1000000

let parse_string ?len {p; freed; gen} buf =
  raise_if_already_freed freed "parse_string" ;
  gen := () ;
  let maxLen = String.(length buf - paddingLen) in
  let len =
    match len with
    | None -> maxLen
    | Some l when l > maxLen -> invalid_arg "parse_string: len"
    | Some l -> l in
  {e= parseString p buf len; gen}

let parse_bytes ?len t buf = parse_string ?len t (Bytes.unsafe_to_string buf)

let parse ?len {p; freed; gen} buf =
  raise_if_already_freed freed "parse" ;
  gen := () ;
  let maxLen = Bigstringaf.(length buf - paddingLen) in
  let len =
    match len with
    | None -> maxLen
    | Some l when l > maxLen -> invalid_arg "parse: len"
    | Some l -> l in
  {e= parse p buf len; gen}

let parse_many ?len ?(batchSize = defaultBatchSize) {p; freed; gen} buf =
  raise_if_already_freed freed "parse_many" ;
  gen := () ;
  let maxLen = Bigstringaf.(length buf - paddingLen) in
  let len =
    match len with
    | None -> maxLen
    | Some l when l > maxLen -> invalid_arg "parse_many: len"
    | Some l -> l in
  {ds= parseMany p buf len batchSize; gen; buf= Some buf}

let load {p; freed; gen} fn =
  raise_if_already_freed freed "load_many" ;
  gen := () ;
  {e= load p fn; gen}

let load_many ?(batchSize = defaultBatchSize) {p; freed; gen} fn =
  raise_if_already_freed freed "load_many" ;
  gen := () ;
  {ds= loadMany p fn batchSize; gen; buf= None}

let seq_of_ds t {ds; gen; _} =
  raise_if_already_freed t.freed "seq_of_ds" ;
  raise_if_different_gen (t.gen != gen) "seq_of_ds" ;
  let iter_end = docStreamIteratorEnd ds in
  let iter = docStreamIteratorBegin ds in
  let rec loop () =
    match docStreamIteratorCompare iter iter_end with
    | false -> Seq.Nil
    | true ->
        let x = {e= docStreamIteratorGet iter; gen} in
        let idx = docStreamIteratorIndex iter in
        Seq.Cons ((idx, x), fun () -> docStreamIteratorNext iter ; loop ())
  in
  loop

let view ({e; gen} : value) =
  match elementType e with
  | '[' ->
      let a = getArray e in
      let len = arraySize a in
      let iter = arrayIterator a in
      let rec loop acc len =
        if len < 0 then List.rev acc
        else
          let e = arrayIteratorGet iter in
          arrayIteratorNext iter ;
          loop ({e; gen} :: acc) (pred len) in
      `A (loop [] (pred len))
  | '{' ->
      let o = getObject e in
      let len = objSize o in
      let iter = objIterator o in
      let rec loop acc len =
        if len < 0 then List.rev acc
        else
          let k, e = objIteratorGet iter in
          objIteratorNext iter ;
          loop ((k, {e; gen}) :: acc) (pred len) in
      `O (loop [] (pred len))
  | 'l' -> `Float (Int64.to_float (getInt64 e))
  | 'u' ->
      let x = getUint64 e in
      let x =
        if x < 0L then Int64.(to_float (neg min_int) +. to_float (neg x))
        else Int64.to_float x in
      `Float x
  | 'd' -> `Float (getDouble e)
  | '"' -> `String (getString e)
  | 't' -> `Bool (getBool e)
  | 'n' -> `Null
  | _ -> assert false

let length ({e; _} : value) =
  match elementType e with
  | '[' ->
      let a = getArray e in
      arraySize a
  | '{' ->
      let o = getObject e in
      objSize o
  | _ -> 1

let repr _ = assert false
let repr_uid = Json_repr.repr_uid ()
let kind {e; _} = elementType e
let bool_exn {e; _} = getBool e
let int64_exn {e; _} = getInt64 e
let uint64_exn {e; _} = getUint64 e
let float_exn {e; _} = getDouble e
let string_exn {e; _} = getString e

let seq_obj_exn {e; gen} =
  let o = getObject e in
  let len = objSize o in
  let iter = objIterator o in
  let pos = ref 0 in
  let rec seq () =
    if !pos = len then Seq.Nil
    else
      let k, e = objIteratorGet iter in
      objIteratorNext iter ;
      incr pos ;
      Seq.Cons ((k, {e; gen}), seq) in
  seq

let seq_arr_exn {e; gen} =
  let o = getArray e in
  let len = arraySize o in
  let iter = arrayIterator o in
  let pos = ref 0 in
  let rec seq () =
    if !pos = len then Seq.Nil
    else
      let e = arrayIteratorGet iter in
      arrayIteratorNext iter ;
      incr pos ;
      Seq.Cons ({e; gen}, seq) in
  seq

let arr_exn f {e; gen} =
  let a = getArray e in
  let len = arraySize a in
  let iter = arrayIterator a in
  Array.init len (fun i ->
      let e = arrayIteratorGet iter in
      arrayIteratorNext iter ;
      f i {e; gen} )

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The ocaml-simdjson programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
