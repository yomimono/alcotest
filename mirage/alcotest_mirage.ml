include Alcotest.Common

let src = Logs.Src.create "Alcotest_mirage" ~doc:"Alcotest framework for MirageOS"
module Log = (val Logs.src_log src : Logs.LOG)

type u = unit Lwt.t

type 'a run = 'a -> u

type 'a test_case = string * speed_level * 'a run

type 'a test = string * 'a test_case list

let show_line msg =
  Log.err (fun f -> f "ASSERT %s" msg)

let fail = Alcotest.Common.fail ~show_line
let failf fmt = Alcotest.Common.failf ~show_line fmt
let check = Alcotest.Common.check ~show_line
let check_raises = Alcotest.Common.check_raises ~show_line

let run name tests =
  let t = empty ~name ~test_dir:"" ~run_id:"" () in
  Lwt_list.iter_s tests
