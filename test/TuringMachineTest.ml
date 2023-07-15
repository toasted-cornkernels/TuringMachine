open Tape
open State
open InfixOperator

module Notebook1 = struct
  let tape = [Zero; One; Zero; One]

  let state = (tape, 3)

  let state_str = string_of_state state

  let _ = print_endline state_str

  let tape_updated = update_tape tape 0 One

  let tape_updated = update_tape tape 1 Zero

  let tape_updated = update_tape tape 10 One

  let _ = "end"
end
