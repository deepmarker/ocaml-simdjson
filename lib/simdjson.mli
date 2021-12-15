(*---------------------------------------------------------------------------
   Copyright (c) 2021 The ocaml-simdjson programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Stdint

type t
type value
type ds

val threadsEnabled : bool lazy_t

val padding : Bigstringaf.t
(** Buffer passed to [parse] or [parseMany] must be padded by
    [padding] bytes (It does not matter what those bytes are
    initialized to). *)

val create : unit -> t
val free : t -> unit
val parse_string : ?len:int -> t -> string -> value
val parse_bytes : ?len:int -> t -> bytes -> value
val parse : ?len:int -> t -> Bigstringaf.t -> value
val load : t -> string -> value
val parse_many : ?len:int -> ?batchSize:int -> t -> Bigstringaf.t -> ds
val load_many : ?batchSize:int -> t -> string -> ds

(* *)
val kind : value -> char
val length : value -> int
val bool_exn : value -> bool
val int64_exn : value -> int64
val uint64_exn : value -> uint64
val float_exn : value -> float
val string_exn : value -> string
val seq_obj_exn : value -> (string * value) Seq.t
val seq_arr_exn : value -> value Seq.t
val arr_exn : (int -> value -> 'a) -> value -> 'a array

val seq_of_ds : t -> ds -> (int * value) Seq.t
(** [elt] must be consumed before iterating further, i.e. after
    iteration, [elt] is invalid and must not be used.*)

val free_ds : ds -> unit

(** Functor compatibility with ocplib-json-typed *)

val view : value -> value Json_repr.view
val repr : value Json_repr.view -> value
val repr_uid : value Json_repr.repr_uid

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
