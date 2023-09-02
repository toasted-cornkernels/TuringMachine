open Tape
open State

let main () =
  let tape : Tape.t = [Zero; One; Zero; One] in
  let tape_string = Tape.to_string tape in
  print_endline tape_string


let () = main ()
