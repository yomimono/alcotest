(*
 * Copyright (c) 2013-2016 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Astring

exception Check_error of string

(* Types *)
type speed_level = [`Quick | `Slow]

type 'a run

type path = Path of (string * int)

type run_result = [
  | `Ok
  | `Exn of path * string * string
  | `Error of path * string
  | `Skip
  | `Todo of string
]

type 'a rrun = 'a -> run_result

type 'a test_case = string * speed_level * 'a run

let test_case n s f = (n, s, f)

type 'a test = string * 'a test_case list

(* global state *)
type 'a t = {

  (* library values. *)
  name : string;
  tests: (path * 'a rrun) list;

  (* caches computed from the library values. *)
  paths: path list;
  doc  : path -> string option;
  speed: path -> speed_level option;

  (* runtime state. *)
  mutable errors: string list;

  (* runtime options. *)
  max_label: int;
  speed_level: speed_level;
  show_errors: bool;
  json       : bool;
  verbose    : bool;
  compact    : bool;
  test_dir   : string;
  run_id     : string;
}

(* TODO the test_dir and run_id arguments aren't super sensible in the mirage case, but I think
   we can just stub them out for now - empty string or whatever *)
let empty ~name ~test_dir ~run_id () =
  let errors = [] in
  let paths = [] in
  let doc _ = None in
  let speed _ = None in
  let tests = [] in
  let max_label = 0 in
  let verbose = false in
  let compact = false in
  let speed_level = `Slow in
  let show_errors = false in
  let json = false in
  { name; errors; tests; paths; doc; speed;
    max_label; speed_level;
    show_errors; json; verbose; compact; test_dir; run_id }

let compare_speed_level s1 s2 =
  match s1, s2 with
  | `Quick, `Quick
  | `Slow , `Slow  -> 0
  | `Quick, _      -> 1
  | _     , `Quick -> -1

let line ppf ?color c =
  let line = String.v ~len:80 (fun _ -> c) in
  match color with
  | Some c -> Fmt.pf ppf "%a\n%!" Fmt.(styled c string)  line
  | None   -> Fmt.pf ppf "%s\n%!"line

let left nb pp ppf a =
  let s = Fmt.to_to_string pp a in
  let nb = nb - String.length s in
  if nb <= 0 then pp ppf a
  else (
    pp ppf a;
    Fmt.string ppf (String.v ~len:nb (fun _ -> ' '))
  )

let print t k = if not t.json then k Fmt.stdout

let string_of_channel ic =
  let n = 32768 in
  let s = Bytes.create n in
  let b = Buffer.create 1024 in
  let rec iter ic b s =
    let nread =
      try input ic s 0 n
      with End_of_file -> 0 in
    if nread > 0 then (
      Buffer.add_substring b (Bytes.unsafe_to_string s) 0 nread;
      iter ic b s
    ) in
  iter ic b s;
  Buffer.contents b

let short_string_of_path (Path (n, i)) = Printf.sprintf "%s.%03d" n i

let file_of_path path ext =
  Printf.sprintf "%s.%s" (short_string_of_path path) ext

let output_dir t =
  Filename.concat t.test_dir t.run_id

let output_file t path =
  Filename.concat (output_dir t) (file_of_path path "output")

let color c ppf fmt = Fmt.(styled c string) ppf fmt
let red_s fmt = color `Red fmt
let red ppf fmt = Fmt.kstrf (fun str -> red_s ppf str) fmt
let green_s fmt = color `Green fmt
let yellow_s fmt = color `Yellow fmt
let bold_s fmt = color `Bold fmt
let cyan_s fmt = color `Cyan fmt

let pp_path t ppf (Path (n, i)) =
  Fmt.pf ppf "%a%3d" (left (t.max_label+8) cyan_s) n i

let doc_of_path t path =
  match t.doc path with
  | None  -> ""
  | Some d -> d

let speed_of_path t path =
  match t.speed path with
  | None   -> `Slow
  | Some s -> s

let print_info t p =
  print t (fun ppf ->
      Fmt.pf ppf "%a   %s" (pp_path t) p (doc_of_path t p)
    )

let left_c = 20 (* ?! oh, maybe for indentation? left column?you are correct! :P
                   that might be agnostic so I'll leave it here, although I don't know how much formatting
                we can do in calls to Logs.debug etc *)

let reset t = print t (fun ppf -> Fmt.string ppf "\r")
let newline t = print t (fun ppf -> Fmt.string ppf "\n")
let print_ch t ch = print t (fun ppf -> Fmt.string ppf ch)

let print_full_result t p = function
  | `Ok            ->
    print t (fun ppf -> left left_c green_s ppf "[OK]");
    print_info t p
  | `Exn _ ->
    print t (fun ppf -> left left_c red_s ppf "[FAIL]");
    print_info t p;
  | `Error _  ->
    print t (fun ppf -> left left_c red_s ppf "[ERROR]");
    print_info t p;
  | `Skip          ->
    print t (fun ppf -> left left_c yellow_s ppf "[SKIP]");
    print_info t p
  | `Todo _        ->
    print t (fun ppf -> left left_c yellow_s ppf "[TODO]");
    print_info t p

let print_compact_result t = function
  | `Exn _   -> print_ch t "F"
  | `Error _ -> print_ch t "E"
  | `Skip    -> print_ch t "S"
  | `Todo _  -> print_ch t "T"
  | `Ok      -> print_ch t "."

let print_event t = function
  | `Start _ when t.compact -> ()
  | `Start p ->
    print t (fun ppf -> left left_c yellow_s ppf " ...");
    print_info t p;
  | `Result (_, r) when t.compact ->
    print_compact_result t r
  | `Result (p, r) ->
    reset t;
    print_full_result t p r;
    newline t

let failure: run_result -> bool = function
  | `Ok
  | `Skip  -> false
  | `Error _
  | `Exn _
  | `Todo _ -> true

let has_run: run_result -> bool = function
  | `Ok
  | `Error _
  | `Exn _ -> true
  | `Skip
  | `Todo _    -> false

let bt () = match Printexc.get_backtrace () with "" -> "" | s  -> "\n" ^ s
let exn path name err =
  let err = Printf.sprintf "%s%s" err (bt ()) in
  `Exn (path, name, err)

let protect_test path (f:'a run): 'a rrun =
  fun args ->
    try f args; `Ok
    with
    | Check_error err ->
      let err = Printf.sprintf "Test error: %s%s" err (bt ()) in
      `Error (path, err)
    | Failure f -> exn path "failure" f
    | Invalid_argument f -> exn path "invalid" f
    | e -> exn path "exception" (Printexc.to_string e)
(* ah, because `error` is in Alcotest_unix now *)
(* I think it's a function, which we can't make abstract... *)
(* we can keep pulling stuff out and stop once it builds *)
             (* yes, exactly :) *)
let skip_fun _ = `Skip

let skip_label (path, _) = path, skip_fun

let filter_test labels (test: path * 'a rrun) =
  let Path (n, i), _ = test in
  match labels with
  | None, _ -> Some test
  | Some m, None   -> if n=m then Some test else None
  | Some m, Some j -> if n=m && j = i then Some test else None

let map_test f l = List.map (fun (path, test) -> path, f path test) l

let filter_tests ~subst path tests =
  let tests = List.fold_left (fun acc test ->
      match filter_test path test with
      | None   -> if subst then skip_label test :: acc else acc
      | Some r -> r :: acc
    ) [] tests in
  List.rev tests

let select_speed t path (f: 'a rrun): 'a rrun =
  if compare_speed_level (speed_of_path t path) t.speed_level >= 0 then
    f
  else
    skip_fun

(* is this all? we can stub that out pretty easily too *)
(* it looks like the invocation that builds this performs the tests in the unix case though,
   which is a little more complicated 
   so maybe not common after all, just the same result structure *)
      (* so maybe do the same thing -- take the stuff *)
type result = {
  success: int;
  failures: int;
  time: float
}

let s = function 0 | 1 -> "" | _ -> "s" (* oh, plurals *)

let is_ascii s = String.for_all Char.Ascii.is_valid s

let err_ascii s =
  let err =
    Printf.sprintf
      "%S is not a valid test label (it should be an ASCII string), skipping." s
  in
  Fmt.(pf stderr) "%a %s\n%!" red "Error:" err

let register t name (ts: 'a test_case list) =
  if not (is_ascii name) then (err_ascii name; t)
  else (
    let max_label = max t.max_label (String.length name) in
    let paths = Hashtbl.create 16 in
    let docs = Hashtbl.create 16 in
    let speeds = Hashtbl.create 16 in
    let ts = List.mapi (fun i (doc, speed, test) ->
        let path = Path (name, i) in
        let doc =
          if doc = "" || doc.[String.length doc - 1] = '.' then doc
          else doc ^ "." in
        Hashtbl.add paths path true;
        Hashtbl.add docs path doc;
        Hashtbl.add speeds path speed;
        path, protect_test path test
      ) ts in
    let tests = t.tests @ ts in
    let paths = Hashtbl.fold (fun k _ acc -> k :: acc) paths [] in
    let paths = t.paths @ paths in
    let doc p = try Some (Hashtbl.find docs p) with Not_found -> t.doc p in
    let speed p = try Some (Hashtbl.find speeds p) with Not_found -> t.speed p in
    { t with paths; tests; doc; speed; max_label; }
  )

exception Test_error

let apply fn t test_dir verbose compact show_errors quick json =
  let show_errors = show_errors in
  let speed_level = if quick then `Quick else `Slow in
  let t = { t with verbose; compact; test_dir; json; show_errors; speed_level } in
  fn t

module type TESTABLE = sig
  type t
  val pp: t Fmt.t
  val equal: t -> t -> bool
end

type 'a testable = (module TESTABLE with type t = 'a)

let pp (type a) (t: a testable) = let (module T) = t in T.pp

let equal (type a) (t: a testable) = let (module T) = t in T.equal

let testable (type a) (pp: a Fmt.t) (equal: a -> a -> bool) : a testable =
  let module M = struct type t = a let pp = pp let equal = equal end
  in (module M)

let int32 = testable Fmt.int32 (=)

let int64 = testable Fmt.int64 (=)

let int = testable Fmt.int (=)

let float eps = testable Fmt.float (fun x y -> abs_float (x -. y) <= eps)

let char = testable Fmt.char (=)

let string = testable Fmt.string (=)

let bool = testable Fmt.bool (=)

let unit = testable (Fmt.unit "()") (=)

let list e =
  let rec eq l1 l2 = match (l1, l2) with
    | (x::xs, y::ys) -> equal e x y && eq xs ys
    | ([], []) -> true
    | _ -> false in
  testable (Fmt.Dump.list (pp e)) eq

let slist (type a) (a : a testable) compare =
  let l = list a in
  let eq l1 l2 = equal l (List.sort compare l1) (List.sort compare l2) in
  testable (pp l) eq

let array e =
  let eq a1 a2 =
    let (m, n) = Array.(length a1, length a2) in
    let rec go i = i = m || (equal e a1.(i) a2.(i) && go (i + 1)) in
    m = n && go 0 in
  testable (Fmt.Dump.array (pp e)) eq

let pair a b =
  let eq (a1, b1) (a2, b2) = equal a a1 a2 && equal b b1 b2 in
  testable (Fmt.Dump.pair (pp a) (pp b)) eq

let option e =
  let eq x y = match (x, y) with
    | (Some a, Some b) -> equal e a b
    | (None, None) -> true
    | _ -> false in
  testable (Fmt.Dump.option (pp e)) eq

let result a e =
  let eq x y = let open Result in
    match (x, y) with
    | (Ok x, Ok y) -> equal a x y
    | (Error x, Error y) -> equal e x y
    | _ -> false in
  testable (Fmt.Dump.result ~ok:(pp a) ~error:(pp e)) eq

let of_pp pp = testable pp (=)

let pass (type a) =
  let module M = struct
    type t = a
    let pp fmt _ = Fmt.string fmt "Alcotest.pass"
    let equal _ _ = true
  end in
  (module M: TESTABLE with type t = M.t)

let reject (type a) =
  let module M = struct
    type t = a
    let pp fmt _ = Fmt.string fmt "Alcotest.reject"
    let equal _ _ = false
  end in
  (module M: TESTABLE with type t = M.t)

let check_err fmt = Format.ksprintf (fun err -> raise (Check_error err)) fmt

let check t ~show_line msg x y =
  show_line msg;
  if not (equal t x y) then
    Fmt.strf "Error %s: expecting@\n%a, got@\n%a." msg (pp t) x (pp t) y
    |> failwith

let fail ~show_line msg =
  show_line msg;
  check_err "Error %s." msg

let failf ~show_line fmt =
  Fmt.kstrf (fail ~show_line) fmt

let neg t = testable (pp t) (fun x y -> not (equal t x y))

let collect_exception f =
  try f (); None with e -> Some e

let check_raises ~show_line msg exn f =
  show_line msg;
  match collect_exception f with
    None ->
    check_err "Fail %s: expecting %s, got nothing." msg (Printexc.to_string exn)
  | Some e ->
    if e <> exn then
      check_err "Fail %s: expecting %s, got %s."
        msg (Printexc.to_string exn) (Printexc.to_string e)
