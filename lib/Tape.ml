open InfixOperator

module TapeSymbol = struct
  type t = Zero | One | Blank [@@deriving equal, compare, sexp]

  let is_zero = function Zero -> true | _ -> false

  let is_one = function One -> true | _ -> false

  let is_blank = function Blank -> true | _ -> false

  let to_string = function Zero -> "0" | One -> "1" | Blank -> "B"
end

type t = TapeSymbol.t list [@@deriving equal]

let to_string : t -> string = String.concat << List.map ~f:TapeSymbol.to_string

(* TODO: Truncate tape as needed with ellipses on both ends *)
