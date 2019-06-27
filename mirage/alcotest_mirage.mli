include Alcotest.S with type u = unit Lwt.t
type 'a run = 'a -> u

val check: 'a testable -> string -> 'a -> 'a -> unit
(** Check that two values are equal. *)

val check_raises: string -> exn -> (unit -> unit) -> unit
(** Check that an exception is raised. *)

val fail: string -> 'a
(** Simply fail. *)

val failf: ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Simply fail with a formatted message. *)

val run : string -> unit test list -> unit Lwt.t
(** [run n t] runs the test suite [t]. [n] is the name of the
    tested library. *)
