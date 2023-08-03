exception TODO

exception SexpParseError

(** Turing machine's state *)
module MachineState = struct
  type t = State of string [@@deriving equal, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp
end

(** The current configuration of machine and tape *)
module State = struct
  type t = MachineState.t * Tape.tape_symbol [@@deriving equal, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp
end

(** The current (current_state, current_tape, current_head_pos) *)
module OverallState = struct
  type t = State.t * Tape.tape * Head.head_pos [@@deriving equal]
end

module Instruction = struct
  type t = Instruction of MachineState.t * Tape.tape_symbol * Head.head_movement | Halt
  [@@deriving equal, sexp]

  let is_halt : t -> bool = function Halt -> true | _ -> false

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp
end

module Transition = struct
  type t = State.t * Instruction.t [@@deriving equal, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp
end

module TransitionTable = struct
  type t = Transition.t list [@@deriving equal, sexp]

  let to_sexp = sexp_of_t

  let of_sexp = t_of_sexp

  let read_transition_table (path : string) : t =
    match Sexp.parse @@ In_channel.read_all path with
    | Done (res, _) ->
        of_sexp res
    | Cont _ ->
        raise SexpParseError
end

let select_instruction (((current_state, current_tape_symbol), _, _) : OverallState.t)
    transition_table : Instruction.t =
  List.Assoc.find_exn transition_table ~equal:State.equal (current_state, current_tape_symbol)


let transition ((_, old_tape, old_head_pos) as old_state : OverallState.t)
    (instruction : Instruction.t) : OverallState.t =
  match instruction with
  | Halt ->
      old_state
  | Instruction (new_state, new_symbol, head_movement) ->
      ( (new_state, new_symbol)
      , Head.update_tape old_tape old_head_pos new_symbol
      , Head.move_head old_head_pos head_movement )
