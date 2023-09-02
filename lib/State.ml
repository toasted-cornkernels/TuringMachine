open Tape
open Head
module F = Format

exception TODO

exception SexpParseError of string

(** Turing machine's state *)
module MachineState = struct
  type t = State of string [@@deriving equal, compare, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp

  let to_string (State str : t) = F.asprintf "State %s" str
end

(** The current configuration of machine and tape *)
module State = struct
  type t = MachineState.t * TapeSymbol.t [@@deriving equal, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp

  let to_string ((machine_state, tape_symbol) : t) =
    F.asprintf "(%s, %s)" (MachineState.to_string machine_state) (TapeSymbol.to_string tape_symbol)
end

(** The current (current_state, current_tape, current_head_pos) *)
module OverallState = struct
  type t = State.t * Tape.t * HeadPosition.t [@@deriving equal]
end

module Instruction = struct
  type t = Instruction of (MachineState.t * TapeSymbol.t * HeadMovement.t) | Halt
  [@@deriving equal, sexp]

  let is_halt : t -> bool = function Halt -> true | _ -> false

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp

  let to_string = function
    | Halt ->
        "Halt"
    | Instruction (machine_state, tape_symbol, movement) ->
        F.asprintf "(%s, %s, %s)"
          (MachineState.to_string machine_state)
          (TapeSymbol.to_string tape_symbol)
          (HeadMovement.to_string movement)
end

module rec Transition : sig
  type t = State.t * Instruction.t [@@deriving equal, sexp]

  val to_sexp : t -> Sexp.t

  val of_sexp : Sexp.t -> t

  val leads_to_halt : t -> TransitionTable.t -> bool
end = struct
  type t = State.t * Instruction.t [@@deriving equal, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp

  let leads_to_halt ((machine_state, _) : t) (transition_table : TransitionTable.t) =
    Instruction.is_halt @@ List.Assoc.find_exn ~equal:State.equal transition_table machine_state
end

and TransitionTable : sig
  type t = Transition.t list [@@deriving equal, sexp]

  val to_sexp : t -> Sexp.t

  val of_sexp : Sexp.t -> t

  val read_from_path : string -> t
end = struct
  type t = Transition.t list [@@deriving equal, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp

  let read_from_path (path : string) : t =
    match Sexp.parse @@ In_channel.read_all path with
    | Done (res, _) ->
        of_sexp res
    | Cont _ ->
        raise @@ SexpParseError (F.asprintf "Sexp parse failed: %s" path)
end

let select_instruction (((current_state, current_tape_symbol), _, _) : OverallState.t)
    transition_table : Instruction.t option =
  List.Assoc.find transition_table ~equal:State.equal (current_state, current_tape_symbol)


(** Continuously transition from a current state until the machine halts. *)
let transition ((_, old_tape, old_head_pos) as old_state : OverallState.t)
    (instruction : Instruction.t) : OverallState.t =
  match instruction with
  | Halt ->
      old_state
  | Instruction (new_state, new_symbol, head_movement) ->
      ( (new_state, new_symbol)
      , Head.update_tape old_tape old_head_pos new_symbol
      , HeadMovement.move_head old_head_pos head_movement )


let rec continuous_transition (current_state : OverallState.t) (transition_table : TransitionTable.t)
    : OverallState.t =
  match select_instruction current_state transition_table with
  | None ->
      current_state
  | Some Halt ->
      current_state
  | Some (Instruction (_, _, _) as instr) ->
      continuous_transition (transition current_state instr) transition_table
