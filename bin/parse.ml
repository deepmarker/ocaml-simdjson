open Core
open Async

module Json = struct
  let of_buf bi_outbuf buffer w buf pos len =
    let msg = Bigstring.To_string.sub buf ~pos ~len in
    let rec loop pos len =
      if len <= 0 then Deferred.unit
      else
        match String.index_from msg pos '\n' with
        | None ->
            Buffer.add_substring buffer msg ~pos ~len ;
            Pipe.pushback w
        | Some idx ->
            let msgLen = idx - pos in
            Buffer.add_substring buffer msg ~pos ~len:msgLen ;
            let contents = Buffer.contents buffer in
            Buffer.clear buffer ;
            let json = Yojson.Safe.from_string ~buf:bi_outbuf contents in
            Pipe.write_without_pushback w json ;
            loop (pos + msgLen + 1) (len - msgLen - 1) in
    loop 0 len

  let of_reader ?(bi_outbuf = Bi_outbuf.create 4096)
      ?(buffer = Buffer.create 4096) r =
    Pipe.create_reader ~close_on_exception:false (fun w ->
        Reader.read_one_chunk_at_a_time r ~handle_chunk:(fun buf ~pos ~len ->
            of_buf bi_outbuf buffer w buf pos len >>| fun () -> `Continue )
        >>= function
        | `Eof -> Deferred.unit
        | `Eof_with_unconsumed_data _ | `Stopped _ -> assert false )
end

let simdjson fn =
  let nb = ref 0 in
  let t = Simdjson.create () in
  let x = Simdjson_async.of_file t fn ~f:(fun _ -> None) in
  Clock_ns.every (Time_ns.Span.of_int_sec 1) (fun () -> printf "%d\n" !nb) ;
  Pipe.iter_without_pushback x ~f:(fun _ -> incr nb)

let simdjsonr r =
  let nb = ref 0 in
  Clock_ns.every (Time_ns.Span.of_int_sec 1) (fun () -> printf "%d\n" !nb) ;
  let t = Simdjson.create () in
  let r = Simdjson_async.of_reader t r ~f:(fun _ -> None) in
  Pipe.iter_without_pushback r ~f:(fun _ -> incr nb)

let yojson r =
  let nb = ref 0 in
  let x = Json.of_reader r in
  Clock_ns.every (Time_ns.Span.of_int_sec 1) (fun () -> printf "%d\n" !nb) ;
  Pipe.iter_without_pushback x ~f:(fun _ -> incr nb)

let simdjson =
  Command.async ~summary:"Parse ndjson file"
    (let open Command.Let_syntax in
    [%map_open
      let fn = anon ("fn" %: string) in
      fun () -> simdjson fn])

let simdjsonr =
  Command.async ~summary:"Parse ndjson file"
    (let open Command.Let_syntax in
    [%map_open
      let fn = anon ("fn" %: string) in
      fun () -> Reader.with_file fn ~buf_len:1000000 ~f:simdjsonr])

let yojson =
  Command.async ~summary:"Parse ndjson file"
    (let open Command.Let_syntax in
    [%map_open
      let fn = anon ("fn" %: string) in
      fun () -> Reader.with_file fn ~f:yojson])

let () =
  Command.group ~summary:"json bench"
    [("yojson", yojson); ("simdjson", simdjson); ("simdjsonr", simdjsonr)]
  |> Command.run
