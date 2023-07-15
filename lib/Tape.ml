open InfixOperator

type tape_symbol = Zero | One | Blank [@@deriving equal, sexp]

type tape = tape_symbol list [@@deriving equal]

let string_of_symbol : tape_symbol -> string = function Zero -> "0" | One -> "1" | Blank -> "B"

let string_of_tape : tape -> string = String.concat << List.map ~f:string_of_symbol

(* TODO: Truncate tape as needed with ellipses on both ends *)
