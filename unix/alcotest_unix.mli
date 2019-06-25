(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Alcotest.S with type u = unit

type 'a run = 'a -> u

val check: 'a testable -> string -> 'a -> 'a -> unit
(** Check that two values are equal. *)

val check_raises: string -> exn -> (unit -> unit) -> unit
(** Check that an exception is raised. *)

val fail: string -> 'a
(** Simply fail. *)

val failf: ('a, Format.formatter, unit, 'b) format4 -> 'a
(** Simply fail with a formatted message. *)

val run: ?and_exit:bool -> ?argv:string array ->
  string -> unit test list -> unit
(** [run n t] runs the test suite [t]. [n] is the name of the
    tested library.

    The optional argument [and_exit] controls what happens when the
    function ends. By default, [and_exit] is set, which makes the
    function exit with [0] if everything is fine or [1] if there is an
    issue. If [and_exit] is [false], then the function raises
    [Test_error] on error.

    The optional argument [argv] specifies the argument sent to
    alcotest like ["--json"], ["--verbose"], etc. (at least one
    argument is required).*)


val run_with_args: ?and_exit:bool -> ?argv:string array ->
  string -> 'a Cmdliner.Term.t -> 'a test list -> unit
(** [run_with_args n a t] Similar to [run a t] but take an extra
    argument [a]. Every test function will receive as argument the
    evaluation of the [Cmdliner] term [a]: this is useful to configure
    the test behaviors using the CLI. *)
