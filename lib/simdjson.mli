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
val uint64_exn : value -> int64
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
