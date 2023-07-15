exception TODO

exception ParseError

type state = State of string [@@deriving equal, sexp]

type machine_state = state * Tape.tape_symbol [@@deriving equal, sexp]

(* (current_state, current_tape, current_head_pos) *)
type overall_state = machine_state * Tape.tape * Head.head_pos [@@deriving equal]

(* (new_machine_state, new_symbol, head_movement) *)
type instruction = machine_state * Tape.tape_symbol * Head.head_movement [@@deriving sexp]

type transition = (machine_state * Tape.tape_symbol) * instruction [@@deriving sexp]

type transition_table = transition list [@@deriving sexp]

let select_instruction (((current_state, current_tape_symbol), _, _) : overall_state)
    transition_table =
  List.Assoc.find_exn transition_table ~equal:equal_machine_state
    (current_state, current_tape_symbol)


let transition ((_, old_tape, old_head_pos) : overall_state)
    ((new_state, new_symbol, head_movement) : instruction) : overall_state =
  ( new_state
  , Head.update_tape old_tape old_head_pos new_symbol
  , Head.move_head old_head_pos head_movement )


let read_transition_table (path : string) =
  let sexp_loaded = Sexp.parse @@ In_channel.read_all path in
  match sexp_loaded with Done (res, _) -> transition_of_sexp res | Cont _ -> raise ParseError
