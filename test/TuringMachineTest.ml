open Tape
open State
open Head
open InfixOperator

module Notebook1 = struct
  let tape = [Zero; One; Zero; One]

  let state = (tape, 3)

  (* let state_str = string_of_state state *)

  (* let _ = print_endline state_str *)

  let tape_updated = update_tape tape (Head_at 0) One

  let tape_updated = update_tape tape (Head_at 1) Zero

  let tape_updated = update_tape tape (Head_at 10) One

  let _ = "end"
end

module Notebook2 = struct
  let sample_state : state = (State "p0", Zero)

  let raw_sexp_sample = sexp_of_state sample_state

  let sexp_sample_str = Sexp.to_string_hum ~indent:2 raw_sexp_sample (* ((State p0) Zero) *)

  (* ^ Don't use `string_of_sexp`! *)

  let _ = state_of_sexp raw_sexp_sample

  let sample_transition : transition = ((State "p0", Zero), (State "p1", One, Right))

  let raw_sexp_sample = sample_transition |> sexp_of_transition |> Sexp.to_string_hum ~indent:2
  (* (((State p0) Zero) ((State p1) One Right)) *)

  let _ = "end"
end
