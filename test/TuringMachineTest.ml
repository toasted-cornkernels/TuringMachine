open Core
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

  let sample_transition : transition = ((State "p0", Zero), Instruction (State "p1", One, Right))

  let raw_sexp_sample = sample_transition |> sexp_of_transition |> Sexp.to_string_hum ~indent:2
  (* (((State p0) Zero) ((State p1) One Right)) *)

  let _ = "end"
end

module Notebook3 = struct
  let goal = "Remaking sample.lisp with the correct format"

  (* type state = machine_state * Tape.tape_symbol [@@deriving equal, sexp] *)
  (* type transition = state * instruction [@@deriving sexp] *)

  let transition_table : transition_table =
    [ ((State "q0", Zero), Instruction (State "q0", Zero, Right))
    ; ((State "q0", One), Instruction (State "q0", One, Right))
    ; ((State "q0", Blank), Instruction (State "a0", Blank, Left))
    ; ((State "q1", Zero), No_op)
    ; ((State "q1", One), No_op)
    ; ((State "q1", Blank), No_op)
    ; ((State "a0", Zero), Instruction (State "s0", Blank, Left))
    ; ((State "a0", One), Instruction (State "s1", Blank, Left))
    ; ((State "a0", Blank), Instruction (State "q1", Blank, Neutral))
    ; ((State "a1", Zero), Instruction (State "s1", Blank, Left))
    ; ((State "a1", One), Instruction (State "s2", Blank, Left))
    ; ((State "a2", Zero), Instruction (State "s2", Blank, Left))
    ; ((State "a2", One), Instruction (State "s0", Blank, Left))
    ; ((State "s0", Zero), Instruction (State "a0", Blank, Left))
    ; ((State "s0", One), Instruction (State "a2", Blank, Left))
    ; ((State "s0", Blank), Instruction (State "q1", Blank, Neutral))
    ; ((State "s1", Zero), Instruction (State "a1", Blank, Left))
    ; ((State "s1", One), Instruction (State "a0", Blank, Left))
    ; ((State "s2", Zero), Instruction (State "a2", Blank, Left))
    ; ((State "s2", One), Instruction (State "a1", Blank, Left)) ]


  let str = Sexp.to_string_hum ~indent:4 @@ sexp_of_transition_table transition_table

  let _ = Out_channel.print_endline str

  (* Sanity check: serializing then deserializing shouldn't affect the content *)
  let _ =
    (* Good! *)
    assert (
      List.equal equal_transition
        (transition_table |> sexp_of_transition_table |> transition_table_of_sexp)
        transition_table )


  let result =
    {|
((((State q0) Zero) (Instruction (State q0) Zero Right))
(((State q0) One) (Instruction (State q0) One Right))
(((State q0) Blank) (Instruction (State a0) Blank Left))
(((State q1) Zero) No_op)
(((State q1) One) No_op)
(((State q1) Blank) No_op)
(((State a0) Zero) (Instruction (State s0) Blank Left))
(((State a0) One) (Instruction (State s1) Blank Left))
(((State a0) Blank) (Instruction (State q1) Blank Neutral))
(((State a1) Zero) (Instruction (State s1) Blank Left))
(((State a1) One) (Instruction (State s2) Blank Left))
(((State a2) Zero) (Instruction (State s2) Blank Left))
(((State a2) One) (Instruction (State s0) Blank Left))
(((State s0) Zero) (Instruction (State a0) Blank Left))
(((State s0) One) (Instruction (State a2) Blank Left))
(((State s0) Blank) (Instruction (State q1) Blank Neutral))
(((State s1) Zero) (Instruction (State a1) Blank Left))
(((State s1) One) (Instruction (State a0) Blank Left))
(((State s2) Zero) (Instruction (State a2) Blank Left))
(((State s2) One) (Instruction (State a1) Blank Left)))|}


  let _ = "end"
end

module Notebook4 = struct
  let goal = "Making `continuous_transition` function"

  let instruction_is_noop : instruction -> bool = function No_op -> true | _ -> false

  let transition_is_noop ((machine_state, _) : transition) (transition_table : transition_table) =
    instruction_is_noop @@ List.Assoc.find_exn ~equal:equal_state transition_table machine_state


  (* TODO: Make instruction and transition their own modules *)

  let _ = "end"
end
