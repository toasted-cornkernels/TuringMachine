open Tape

exception TODO

exception InvalidState

type head_pos = Head_at of int [@@deriving equal]

type head_movement = Left | Right | Neutral [@@deriving equal, sexp]

let string_of_head_pos (Head_at head_pos : head_pos) =
  String.init (head_pos - 1) ~f:(fun _ -> ' ') ^ "^"


let move_head (Head_at head_pos : head_pos) = function
  | Left ->
      Head_at (head_pos - 1)
  | Right ->
      Head_at (head_pos + 1)
  | Neutral ->
      Head_at head_pos


let update_tape (tape : tape) (Head_at head_pos : head_pos) (symbol : tape_symbol) : tape =
  if head_pos > List.length tape then raise InvalidState
  else
    let tape_prefix, tape_suffix = List.split_n tape head_pos in
    let tape_suffix_updated = List.cons symbol @@ List.drop tape_suffix 1 in
    List.append tape_prefix tape_suffix_updated
