open Tape
open State

let main () =
  let tape = [Zero; One; Zero; One] in
  let tape_string = string_of_tape tape in
  print_endline tape_string


let () = main ()
