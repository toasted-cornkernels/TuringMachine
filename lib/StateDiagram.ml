open Tape
open Head
open State

module EdgeLabel : Graph.Sig.ORDERED_TYPE_DFT = struct
  (* The edge label is the pair of (current symbol, new symbol, head movement) *)
  type t = TapeSymbol.t * TapeSymbol.t * HeadMovement.t [@@deriving equal, compare]

  let default : t = (Zero, Zero, Left)
end

module Vertex : Graph.Sig.COMPARABLE = struct
  include MachineState

  let hash = Hashtbl.hash
end

module Internal = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (EdgeLabel)

type t = Internal.t
