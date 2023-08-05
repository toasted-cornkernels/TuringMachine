open Core
open Tape
open State
open Head
open InfixOperator

module Notebook1 = struct
  let tape : Tape.t = [Zero; One; Zero; One]

  let state = (tape, 3)

  (* let state_str = string_of_state state *)

  (* let _ = print_endline state_str *)

  let tape_updated = update_tape tape (Head_at 0) One

  let tape_updated = update_tape tape (Head_at 1) Zero

  let tape_updated = update_tape tape (Head_at 10) One

  let _ = "end"
end

module Notebook2 = struct
  let sample_state : State.t = (State "p0", Zero)

  let raw_sexp_sample : Sexp.t = State.to_sexp sample_state

  let sexp_sample_str = Sexp.to_string_hum ~indent:2 raw_sexp_sample (* ((State p0) Zero) *)

  (* ^ Don't use `string_of_sexp`! *)

  let _ = State.of_sexp raw_sexp_sample

  let sample_transition : Transition.t = ((State "p0", Zero), Instruction (State "p1", One, Right))

  let raw_sexp_sample = sample_transition |> Transition.to_sexp |> Sexp.to_string_hum ~indent:2
  (* (((State p0) Zero) ((State p1) One Right)) *)

  let _ = "end"
end

module Notebook3 = struct
  let goal = "Remaking sample.lisp with the correct format"

  (* type state = machine_state * Tape.tape_symbol [@@deriving equal, sexp] *)
  (* type transition = state * instruction [@@deriving sexp] *)

  let transition_table : TransitionTable.t =
    [ ((State "q0", Zero), Instruction (State "q0", Zero, Right))
    ; ((State "q0", One), Instruction (State "q0", One, Right))
    ; ((State "q0", Blank), Instruction (State "a0", Blank, Left))
    ; ((State "q1", Zero), Halt)
    ; ((State "q1", One), Halt)
    ; ((State "q1", Blank), Halt)
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


  let str = Sexp.to_string_hum ~indent:4 @@ TransitionTable.to_sexp transition_table

  let _ = Out_channel.print_endline str

  (* Sanity check: serializing then deserializing shouldn't affect the content *)
  let _ =
    (* Good! *)
    assert (
      List.equal Transition.equal
        (transition_table |> TransitionTable.to_sexp |> TransitionTable.of_sexp)
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

  let instruction_is_noop : Instruction.t -> bool = function Halt -> true | _ -> false

  let transition_is_noop ((machine_state, _) : Transition.t) (transition_table : TransitionTable.t)
      =
    instruction_is_noop @@ List.Assoc.find_exn ~equal:State.equal transition_table machine_state


  let sample_transition : Transition.t =
    Transition.of_sexp @@ Sexp.of_string {|(((State s1) Zero) (Instruction (State a1) Blank Left))|}


  let _ = transition_is_noop sample_transition Notebook3.transition_table

  let noop_transition : Transition.t =
    Transition.of_sexp @@ Sexp.of_string {|(((State q1) Zero) No_op)|}


  let _ = transition_is_noop noop_transition Notebook3.transition_table

  (* Works pretty well! *)

  (** Continuously transition from a current state until the machine halts. *)
  let rec continuous_transition (current_state : OverallState.t)
      (transition_table : TransitionTable.t) : OverallState.t =
    match select_instruction current_state transition_table with
    | Halt ->
        current_state
    | Instruction (_, _, _) as instr ->
        continuous_transition (transition current_state instr) transition_table


  let sample_transition_table : TransitionTable.t =
    [ ((State "p0", Zero), Instruction (State "p1", Zero, Right))
    ; ((State "p1", Zero), Instruction (State "p2", Zero, Right))
    ; ((State "p2", Zero), Instruction (State "p3", Zero, Right))
    ; ((State "p3", Zero), Halt) ]


  let sample_current_state : OverallState.t =
    ((State "p0", Zero), [Zero; Zero; Zero; Zero; Zero], Head_at 0)


  (* TODO: Debug `continuous_transition` *)

  let _ = "end"
end

(* TODO 1: GraphViz representation of transitiontable.t *)
(* TODO 2: If List.Assoc.find_exn fails, then treat it as halt *)
(* TODO 3: Tape extending functionality (in both directions) to simulate infinite tape *)
