exception TODO

exception ParseError

(** Turing machine's state *)
type machine_state = State of string [@@deriving equal, sexp]

(** The current configuration of machine and tape *)
type state = machine_state * Tape.tape_symbol [@@deriving equal, sexp]

(** The current (current_state, current_tape, current_head_pos) *)
type overall_state = state * Tape.tape * Head.head_pos [@@deriving equal]

(* (new_machine_state, new_symbol, head_movement) *)
type instruction = Instruction of machine_state * Tape.tape_symbol * Head.head_movement | No_op
[@@deriving equal, sexp]

type transition = state * instruction [@@deriving equal, sexp]

type transition_table = transition list [@@deriving equal, sexp]

let select_instruction (((current_state, current_tape_symbol), _, _) : overall_state)
    transition_table : instruction =
  List.Assoc.find_exn transition_table ~equal:equal_state (current_state, current_tape_symbol)


let transition ((_, old_tape, old_head_pos) as old_state : overall_state) (instruction : instruction)
    : overall_state =
  match instruction with
  | No_op ->
      old_state
  | Instruction (new_state, new_symbol, head_movement) ->
      ( (new_state, new_symbol)
      , Head.update_tape old_tape old_head_pos new_symbol
      , Head.move_head old_head_pos head_movement )


let read_transition_table (path : string) : transition =
  let sexp_loaded = Sexp.parse @@ In_channel.read_all path in
  match sexp_loaded with Done (res, _) -> transition_of_sexp res | Cont _ -> raise ParseError
