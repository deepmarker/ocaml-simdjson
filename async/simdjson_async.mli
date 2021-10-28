(*---------------------------------------------------------------------------
   Copyright (c) 2021 The ocaml-simdjson programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Core
open Async
open Simdjson

val of_reader :
  ?exn:exn Ivar.t ->
  ?buflen:int ->
  t ->
  Reader.t ->
  f:(value -> 'a option) ->
  (int * 'a) Pipe.Reader.t

val of_pipe :
  ?exn:exn Ivar.t ->
  ?buflen:int ->
  t ->
  (Bigstring.t * int * int) Pipe.Reader.t ->
  f:(value -> 'a option) ->
  (int * 'a) Pipe.Reader.t

val of_file :
  ?exn:exn Ivar.t ->
  ?batchSize:int ->
  t ->
  string ->
  f:(value -> 'a option) ->
  (int * 'a) Pipe.Reader.t

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
