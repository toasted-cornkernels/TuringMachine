open Tape
open Head
open State

module EdgeLabel = struct
  (* The edge label is the pair of (current symbol, new symbol, head movement) *)
  type t = TapeSymbol.t * TapeSymbol.t * HeadMovement.t [@@deriving equal, compare]

  let default : t = (Zero, Zero, Left)
end

module Vertex = struct
  include MachineState

  let hash = Hashtbl.hash
end

include Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (EdgeLabel)

let graph_attributes (g : t) = [`Label "I am a Turing Machine"]

let default_vertex_attributes _ = []

let vertex_name (vertex : V.t) = Vertex.to_string vertex

let vertex_attributes (vertex : V.t) = raise TODO

let get_subgraph _ = None

let default_edge_attributes _ = []

let edge_attributes ((_, label, _) : E.t) = raise TODO

let pp_vertex = vertex_name

let pp_edge ((v1, v2) : V.t * V.t) = F.asprintf "\"(%s, %s)\"" (vertex_name v1) (vertex_name v2)
