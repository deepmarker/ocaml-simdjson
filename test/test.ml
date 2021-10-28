open Alcotest
open Simdjson
module Simdjson_encoding = Json_encoding.Make (Simdjson)

let p = create ()
let buf = Bigstringaf.create 4096

let destruct_safe encoding value =
  try Simdjson_encoding.destruct encoding value
  with exn ->
    Format.eprintf "%a@." (Json_encoding.print_error ?print_unknown:None) exn ;
    raise exn

let padding = String.make 32 '\x00'
let subs json = json ^ padding

let bigSubs json =
  let len = String.length json in
  Bigstringaf.blit_from_string json ~src_off:0 buf ~dst_off:0 ~len ;
  Bigstringaf.sub buf ~off:0 ~len:(32 + len)

let obj0 () =
  let json = {|[]|} in
  let subs = subs json in
  let elt = parse_string p subs in
  let _ = view elt in
  Gc.compact () ; ()

let obj () =
  let encoding = Json_encoding.(obj2 (req "object" bool) (req "machin" float)) in
  let json = {|{"object": true, "machin": 3.23}|} in
  let subs = subs json in
  let elt = parse_string p subs in
  let b, fl = destruct_safe encoding elt in
  Gc.compact () ;
  check bool "b" true b ;
  check (float 0.1) "f" 3.23 fl

let bigObj fn =
  let a =
    let open Json_encoding in
    let level = tup2 string string in
    obj3 (req "lastUpdateId" int53)
      (req "bids" (array level))
      (req "asks" (array level)) in
  let elt = load p fn in
  let _ = destruct_safe a elt in
  ()

let parseMany () =
  let open Simdjson in
  let a = {|["0","1","2"]["0","1","2"]|} in
  let b = {|["0","1","2"]["0","1","2"]|} in
  let c = {|["0","1","2"]    ["0","1","2"]  |} in
  let d = "[\"0\"]\n[\"0\"]" in
  let e = "[\"0\"]\r\n[\"0\"]" in
  let loop json =
    let subs = bigSubs json in
    let ds = parse_many p subs in
    let enc = Json_encoding.(array string) in
    let on_elt acc (_, x) =
      Gc.compact () ;
      Simdjson_encoding.destruct enc x :: acc in
    let _ = Seq.fold_left on_elt [] (seq_of_ds p ds) in
    () in
  List.iter loop [a; b; c; d; e]

let loadMany () =
  let fn = Filename.temp_file "simdjson" "test" in
  let oc = open_out fn in
  output_string oc {|{"type": "pong"}|} ;
  output_char oc '\n' ;
  output_string oc {|{"type": "subscribed", "channel": "markets"}|} ;
  output_char oc '\n' ;
  close_out oc ;
  let ds = load_many p fn in
  Seq.fold_left
    (fun a (i, _) ->
      match a with [] -> assert false | h :: t -> check int "" h i ; t )
    [0; 17] (seq_of_ds p ds)
  |> fun _ -> Sys.remove fn

let loadMany2 fn () =
  let ds = load_many p fn in
  let seq = seq_of_ds p ds in
  Gc.compact () ;
  Seq.iter (fun x -> ignore x) seq

let fn = ["2020-11-10_22-53-43.426713.raw.tmp"]
let loadManyX = List.map (fun fn -> (fn, `Quick, loadMany2 fn)) fn

let bigObj fn () =
  let _x = load p fn in
  ()

let bigs = ["exchangeInfo.json"]
let bigsX = List.map (fun fn -> (fn, `Quick, bigObj fn)) bigs
let thread () = check bool "" true (Lazy.force threadsEnabled)

let basic =
  [ test_case "threads" `Quick thread; ("obj0", `Quick, obj0);
    ("obj", `Quick, obj); ("bigObj", `Quick, bigObj "depth.json");
    ("parseMany", `Quick, parseMany); ("loadMany", `Quick, loadMany) ]

let () =
  run "simdjson" [("basic", basic); ("loadMany", loadManyX); ("bigs", bigsX)]
