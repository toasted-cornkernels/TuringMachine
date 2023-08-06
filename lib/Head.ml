open Tape

exception TODO

exception InvalidState

module F = Format

module HeadPosition = struct
  type t = Head_at of int [@@deriving equal]

  let to_string (Head_at int : t) = F.asprintf "Head_at %d" int

  let pp (Head_at head_pos : t) = String.init (head_pos - 1) ~f:(fun _ -> ' ') ^ "^"
end

module HeadMovement = struct
  type t = Left | Right | Neutral [@@deriving equal, compare, sexp]

  let is_left = function Left -> true | _ -> false

  let is_right = function Right -> true | _ -> false

  let is_neutral = function Neutral -> true | _ -> false

  let move_head (Head_at head_pos : HeadPosition.t) (movement : t) : HeadPosition.t =
    match movement with
    | Left ->
        Head_at (head_pos - 1)
    | Right ->
        Head_at (head_pos + 1)
    | Neutral ->
        Head_at head_pos


  let to_string = function Left -> "Left" | Right -> "Right" | Neutral -> "Neutral"
end

let update_tape (tape : Tape.t) (Head_at head_pos : HeadPosition.t) (symbol : TapeSymbol.t) : Tape.t
    =
  if head_pos > List.length tape then raise InvalidState
  else
    let tape_prefix, tape_suffix = List.split_n tape head_pos in
    let tape_suffix_updated = List.cons symbol @@ List.drop tape_suffix 1 in
    List.append tape_prefix tape_suffix_updated
