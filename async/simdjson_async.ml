(*---------------------------------------------------------------------------
   Copyright (c) 2021 The ocaml-simdjson programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Core
open Async
open Simdjson

let parse_many ?(sep = '\n') ?(buflen = 4096) t w f =
  let bbuf = Bigbuffer.create buflen in
  fun buf ~pos ~len ->
    let rev_find buf pos len =
      let rec loop p =
        if p < 0 then p
        else if Char.equal (Bigstring.get buf (pos + p)) sep then p
        else loop (pred p) in
      loop (pred len) in
    let len' = rev_find buf pos len in
    if len' < 0 then (
      Bigbuffer.add_bigstring bbuf (Bigstring.sub_shared buf ~pos ~len) ;
      Deferred.unit )
    else (
      Bigbuffer.add_bigstring bbuf (Bigstring.sub_shared buf ~pos ~len:len') ;
      let parseLen = Bigbuffer.length bbuf in
      Bigbuffer.add_bigstring bbuf padding ;
      let ds = parse_many ~len:parseLen t (Bigbuffer.volatile_contents bbuf) in
      let seq = seq_of_ds t ds in
      Seq.iter
        (fun (i, e) ->
          match f e with
          | None -> ()
          | Some x -> Pipe.write_without_pushback_if_open w (i, x) )
        seq ;
      free_ds ds ;
      Pipe.pushback w
      >>| fun () ->
      Bigbuffer.clear bbuf ;
      if len - len' - 1 > 0 then
        Bigbuffer.add_bigstring bbuf
          (Bigstring.sub_shared buf ~pos:(pos + len' + 1) ~len:(len - len' - 1))
      )

let of_reader ?exn ?buflen t r ~f =
  Pipe.create_reader ~close_on_exception:false (fun w ->
      let parse = parse_many ?buflen t w f in
      let handle_chunk buf ~pos ~len =
        Monitor.try_with ~extract_exn:true (fun () -> parse buf ~pos ~len)
        >>= function Error e -> return (`Stop e) | Ok () -> return `Continue
      in
      Reader.read_one_chunk_at_a_time r ~handle_chunk
      >>= function
      | `Stopped e ->
          Option.iter exn ~f:(fun exn -> Ivar.fill_if_empty exn e) ;
          Deferred.unit
      | _ -> Deferred.unit )

let of_pipe ?exn ?buflen t r ~f =
  Pipe.create_reader ~close_on_exception:false (fun w ->
      let parse = parse_many ?buflen t w f in
      let rec loop () =
        Pipe.read r
        >>= function
        | `Eof -> Deferred.unit
        | `Ok (buf, pos, len) -> (
            Monitor.try_with ~extract_exn:true (fun () -> parse buf ~pos ~len)
            >>= function
            | Error e ->
                Option.iter exn ~f:(fun exn -> Ivar.fill_if_empty exn e) ;
                Deferred.unit
            | Ok () -> loop () ) in
      loop () )

let of_file ?exn ?batchSize t fn ~f =
  let ds = load_many ?batchSize t fn in
  let seq = seq_of_ds t ds in
  let r =
    Pipe.create_reader ~close_on_exception:false (fun w ->
        let rec loop seq =
          match seq () with
          | exception e ->
              Option.iter exn ~f:(fun exn -> Ivar.fill_if_empty exn e) ;
              Deferred.unit
          | Seq.Nil -> Deferred.unit
          | Cons ((i, e), seq) -> (
            match f e with
            | None -> loop seq
            | Some x -> (
              match i mod 1000 with
              | 0 -> Pipe.write_if_open w (i, x) >>= fun () -> loop seq
              | _ ->
                  Pipe.write_without_pushback_if_open w (i, x) ;
                  loop seq ) ) in
        loop seq ) in
  don't_wait_for (Pipe.closed r >>| fun () -> free_ds ds) ;
  r

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
