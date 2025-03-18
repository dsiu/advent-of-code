module Serializable = StdlibFp.Serializable
module JSONSerializable = StdlibFp.JSONSerializable

module type S = {
  type node

  type t

  let make: unit => t

  let addNode: (t, node) => unit
  let removeNode: (t, node) => bool
  let getNode: (t, node) => array<node>

  let addUndirectedEdge: (t, node, node) => unit
  let addDirectedEdge: (t, node, node) => unit
  let removeUndirectedEdge: (t, node, node) => bool
  let removeDirectedEdge: (t, node, node) => bool

  let adjacent: (t, node, node) => bool

  let neighbors: (t, node) => array<node>
}

module type T = {
  type t
}

module MakeImpl = (NodeC: StdlibFp.Map.S): (
  S with type node = NodeC.key and type t = NodeC.t<NodeC.key, NodeC.t<NodeC.key, int>>
) => {
  type node = NodeC.key
  //  type edgeContainer = EdgeC.t<NodeC.key>
  type t = NodeC.t<NodeC.key, NodeC.t<NodeC.key, int>>

  let make = NodeC.make

  let weight = 0

  let addNode = (t, node) => {
    switch t->NodeC.get(node) {
    | Some(_) => ()
    | None => t->NodeC.set(node, NodeC.make())
    }
  }
  let removeNode = (t, node) => t->NodeC.delete(node)

  let getNode = (t, node) => {
    switch t->NodeC.get(node) {
    | Some(ec) => ec->NodeC.keys->Iterator.toArray
    | None => raise(Not_found)
    }
  }

  let addDirectedEdge = (t, a, b) => {
    addNode(t, a)
    addNode(t, b)
    switch t->NodeC.get(a) {
    | Some(ec) => ec->NodeC.set(b, weight)
    | None => raise(Not_found)
    }
  }

  let addUndirectedEdge: (t, node, node) => unit = (t, a, b) => {
    addDirectedEdge(t, a, b)
    addDirectedEdge(t, b, a)
  }

  let removeUndirectedEdge = (t, a, b) => {
    switch t->NodeC.get(a) {
    | Some(ec) => ec->NodeC.delete(b)
    | None => false
    } &&
    switch t->NodeC.get(b) {
    | Some(ec) => ec->NodeC.delete(a)
    | None => false
    }
  }

  let removeDirectedEdge = (t, a, b) => {
    switch t->NodeC.get(a) {
    | Some(ec) => ec->NodeC.delete(b)
    | None => false
    }
  }

  let adjacent = (t, a, b) => {
    switch t->NodeC.get(a) {
    | Some(ec) => ec->NodeC.has(b)
    | None => false
    }
  }

  let neighbors = (t, node) => {
    t->NodeC.get(node)->Option.getOr(NodeC.make())->NodeC.keys->Iterator.toArray
  }
}

module MakeWithPrimitive = (T: T): (S with type node = T.t) => {
  module NodeC = StdlibFp.Map.MakeWithPrimitive(T)
  //  module EdgeC = StdlibFp.Set.MakeWithPrimitive(T)
  include MakeImpl(NodeC)
}

module Make = (Serializable: Serializable.S): (S with type node = Serializable.t) => {
  module NodeC = StdlibFp.Map.Make(Serializable)
  //  module EdgeC = StdlibFp.Set.Make(Serializable)

  include MakeImpl(NodeC)
}

module Node = {
  module String = MakeWithPrimitive({
    type t = string
  })

  module Tuple2 = {
    module Make = (A: JSONSerializable.S, B: JSONSerializable.S) => Make(
      Serializable.MakeTuple2(A, B),
    )
    module IntInt = Make(JSONSerializable.Int, JSONSerializable.Int)
    module StringInt = Make(JSONSerializable.String, JSONSerializable.Int)
  }
}
