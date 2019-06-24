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

include Alcotest.Common

let mkdir_p path mode =
  let is_win_drive_letter x =
    String.length x = 2
    && x.[1] = ':'
    && Char.Ascii.is_letter x.[0]
  in
  let sep = Filename.dir_sep in
  let rec mk parent = function
  | [] -> ()
  | name::names ->
      let path = parent ^ sep ^ name in
      begin
        try Unix.mkdir path mode
        with Unix.Unix_error(Unix.EEXIST, _, _) ->
          if Sys.is_directory path then
            () (* the directory exists *)
          else
            Fmt.strf "mkdir: %s: is a file" path |> failwith
      end;
      mk path names in
  match String.cuts ~empty:true ~sep:sep path with
  | ""::xs -> mk sep xs
  (* check for Windows drive letter *)
  | dl::xs when is_win_drive_letter dl -> mk dl xs
  | xs -> mk "." xs

let prepare t =
  let test_dir = output_dir t in
  if not (Sys.file_exists test_dir) then mkdir_p test_dir 0o755

(* this is pretty unix-y though :( *)
let error t path fmt =
  Fmt.kstrf (fun error ->
      let logs =
        let filename = output_file t path in
        if t.verbose || not (Sys.file_exists filename)
        then Fmt.strf "%s\n" error
        else
          let file = open_in filename in
          let output = string_of_channel file in
          close_in file;
          Fmt.strf "in %s:\n%s" filename output
      in
      let error =
        Fmt.strf "-- %s [%s] Failed --\n%s"
          (short_string_of_path path) (doc_of_path t path) logs
      in
      t.errors <- error :: t.errors;
    ) fmt

let perform_test t args (path, test) =
  print_event t (`Start path);
  let result = test args in
  (* Store errors *)
  let () = match result with
    | `Exn (p, n, s) -> error t p "[%s] %s" n s
    | `Error (p, s)  -> error t p "%s" s
    | _ -> ()
  in
  print_event t (`Result (path, result));
  result

let perform_tests t tests args = List.map (perform_test t args) tests

let with_redirect file fn =
  flush stdout;
  flush stderr;
  let fd_stdout = Unix.descr_of_out_channel stdout in
  let fd_stderr = Unix.descr_of_out_channel stderr in
  let fd_old_stdout = Unix.dup fd_stdout in
  let fd_old_stderr = Unix.dup fd_stderr in
  let fd_file = Unix.(openfile file [O_WRONLY; O_TRUNC; O_CREAT] 0o666) in
  Unix.dup2 fd_file fd_stdout;
  Unix.dup2 fd_file fd_stderr;
  Unix.close fd_file;
  let r =
    try `Ok (fn ())
    with e -> `Error e in
  flush stdout;
  flush stderr;
  Unix.dup2 fd_old_stdout fd_stdout;
  Unix.dup2 fd_old_stderr fd_stderr;
  Unix.close fd_old_stdout;
  Unix.close fd_old_stderr;
  match r with
  | `Ok x -> x
  | `Error e -> raise e


let redirect_test_output t path (f: 'a rrun) =
  if t.verbose then f
  else fun args ->
    let output_file = output_file t path in
    with_redirect output_file (fun () ->
      let result = f args in
      begin match result with
        | `Error (_path, str) -> Printf.printf "%s\n" str
        | `Exn (_path, n, str) -> Printf.printf "[%s] %s\n" n str
        | `Ok | `Todo _ | `Skip -> ()
      end;
      result
    )

(* Return the json for the api, dirty out, to avoid new dependencies *)
let json_of_result r =
  Printf.sprintf "{\"success\":%i,\"failures\":%i,\"time\":%f}"
    r.success r.failures r.time

let show_result t result =
  (* Function to display errors for each test *)
  let display_errors () = match result.failures with
    | 0 -> ()
    | _ ->
      if result.failures > 0 then
        let print_error error = Printf.printf "%s\n" error in
        if t.verbose || t.show_errors then
          List.iter print_error (List.rev t.errors)
        else
          print_error (List.hd (List.rev t.errors))
  in
  match t.json with
  | true  -> Printf.printf "%s\n" (json_of_result result)
  | false ->
    if t.compact then newline t;
    display_errors ();
    let test_results ppf = match result.failures with
      | 0 -> green_s ppf "Test Successful"
      | n -> red     ppf "%d error%s!" n (s n)
    in
    let full_logs ppf =
      if t.verbose then Fmt.string ppf ""
      else
        Fmt.pf ppf "The full test results are available in `%s`.\n"
          (output_dir t)
    in
    if (not t.compact || result.failures > 0) then
      Fmt.pr "%t%t in %.3fs. %d test%s run.\n%!"
        full_logs test_results result.time result.success (s result.success)

(* maybe we can factor some stuff out here? there must be a record definition for this, which
   maybe can be common, even though a lot of this stuff is unix-y *)
let make_result t test args =
  prepare t;
  let start_time = Sys.time () in
  let test = map_test (redirect_test_output t) test in
  let test = map_test (select_speed t) test in
  let results = perform_tests t test args in
  let time = Sys.time () -. start_time in
  let success = List.length (List.filter has_run results) in
  let failures = List.filter failure results in
  { time; success; failures = List.length failures }

let run_registered_tests t () args =
  let result = make_result t t.tests args in
  show_result t result;
  result.failures

let run_subtest t labels () args =
  let is_empty = filter_tests ~subst:false labels t.tests = [] in
  if is_empty then (
    Fmt.(pf stderr) "%a\n" red "Invalid request (no tests to run, filter skipped everything)!";
    exit 1
  ) else
    let tests = filter_tests ~subst:true labels t.tests in
    let result = make_result t tests args in
    show_result t result;
    result.failures

let list_tests t () =
  let paths = List.sort Pervasives.compare t.paths in
  List.iter (fun path ->
      Fmt.(pf stdout) "%a    %s\n" (pp_path t) path (doc_of_path t path)
    ) paths;
  0

(* all this cmdliner stuff is going to be unix-only, since mirage will handle that kind of thing via bootvars (if we even have any that make sense) *)
open Cmdliner

let json =
  let doc = "Display JSON for the results, to be used by a script." in
  Arg.(value & flag & info ["json"] ~docv:"" ~doc)

let test_dir =
  let fname_concat l = List.fold_left Filename.concat "" l in
  let default_dir = fname_concat [Sys.getcwd (); "_build"; "_tests"] in
  let doc = "Where to store the log files of the tests." in
  Arg.(value & opt dir default_dir & info ["o"] ~docv:"DIR" ~doc)

let verbose =
  let env = Arg.env_var "ALCOTEST_VERBOSE" in
  let doc =
    "Display the test outputs. $(b,WARNING:) when using this option \
     the output logs will not be available for further inspection."
 in
  Arg.(value & flag & info ~env ["v"; "verbose"] ~docv:"" ~doc)

let compact =
  let env = Arg.env_var "ALCOTEST_COMPACT" in
  let doc =
    "Compact the output of the tests"
 in
  Arg.(value & flag & info ~env ["c"; "compact"] ~docv:"" ~doc)

let show_errors =
  let env = Arg.env_var "ALCOTEST_SHOW_ERRORS" in
  let doc = "Display the test errors." in
  Arg.(value & flag & info ~env ["e"; "show-errors"] ~docv:"" ~doc)

let quicktests =
  let env = Arg.env_var "ALCOTEST_QUICK_TESTS" in
  let doc = "Run only the quick tests." in
  Arg.(value & flag & info ~env ["q"; "quick-tests"] ~docv:"" ~doc)

let of_env t =
  Term.(pure (apply (fun t -> t) t)
        $ test_dir $ verbose $ compact $ show_errors $ quicktests $ json)

let set_color style_renderer =
  Fmt_tty.setup_std_outputs ?style_renderer ()

let set_color = Term.(const set_color $ Fmt_cli.style_renderer ())

let default_cmd t args =
  let doc = "Run all the tests." in
  Term.(pure run_registered_tests $ of_env t $ set_color $ args),
  Term.info t.name ~version:"%%VERSION%%" ~doc

let test_cmd t args =
  let doc = "Run a given test." in
  let testname =
    let doc = "The label (name) of the test identifying a subset of the tests to run" in
    Arg.(value & pos 0 (some string) None & info [] ~doc ~docv:"NAME")
  in
  let testcase =
    let doc = "The test case number identifying a single test to run" in
    Arg.(value & pos 1 (some int) None & info [] ~doc ~docv:"TESTCASE")
  in
  let label = Term.(pure (fun n t -> n, t) $ testname $ testcase) in
  Term.(pure run_subtest $ of_env t $ label $ set_color $ args),
  Term.info "test" ~doc

let list_cmd t =
  let doc = "List all available tests." in
  Term.(pure list_tests $ of_env t $ set_color),
  Term.info "list" ~doc

let random_state = Random.State.make_self_init ()

let run_with_args ?(and_exit = true) ?argv name args (tl: 'a test list) =
  let run_id =
    Uuidm.v4_gen random_state ()
    |> Uuidm.to_string ~upper:true in
  Fmt.(pf stdout) "Testing %a.\n" bold_s name;
  Fmt.(pf stdout) "This run has ID `%s`.\n" run_id;
  let test_dir = Sys.getcwd () in
  let t = empty ~name ~run_id ~test_dir () in
  let t = List.fold_left (fun t (name, tests) -> register t name tests) t tl in
  let choices = [
    list_cmd t;
    test_cmd t args;
  ] in
  match Term.eval_choice ?argv (default_cmd t args) choices with
  | `Ok 0    -> if and_exit then exit 0 else ()
  | `Error _ -> if and_exit then exit 1 else raise Test_error
  | `Ok i    -> if and_exit then exit i else raise Test_error
  | _        -> if and_exit then exit 0 else ()

let run ?and_exit ?argv name tl =
  run_with_args ?and_exit ?argv name (Term.pure ()) tl

let show_line msg =
  line Fmt.stderr ~color:`Yellow '-';
  Printf.eprintf "ASSERT %s\n" msg;
  line Fmt.stderr ~color:`Yellow '-'

let line (oc:out_channel) ?color c =
  let color = match color with
    | None         -> None
    | Some `Blue   -> Some `Cyan
    | Some `Yellow -> Some `Yellow
  in
  let str: string = Fmt.(to_to_string @@ fun ppf -> line ppf ?color) c in
  Printf.fprintf oc "%s" str

let fail = Alcotest.Common.fail ~show_line
let failf fmt = Alcotest.Common.failf ~show_line fmt
let check = Alcotest.Common.check ~show_line
let check_raises = Alcotest.Common.check_raises ~show_line

let () = at_exit (Format.pp_print_flush Format.err_formatter)
