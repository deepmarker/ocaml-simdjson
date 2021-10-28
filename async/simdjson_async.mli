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
