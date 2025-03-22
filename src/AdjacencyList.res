module Serializable = StdlibFp.Serializable
module JSONSerializable = StdlibFp.JSONSerializable

module type S = {
  type node

  type t<'a>

  let make: unit => t<'a>

  let addNode: (t<'a>, node) => unit
  let removeNode: (t<'a>, node) => bool
  let hasNode: (t<'a>, node) => bool
  let getAllNodes: t<'a> => array<node>

  let addUndirectedEdge: (t<'a>, node, node, ~weight: option<'a>=?) => unit
  let addDirectedEdge: (t<'a>, node, node, ~weight: option<'a>=?) => unit
  let removeUndirectedEdge: (t<'a>, node, node) => bool
  let removeDirectedEdge: (t<'a>, node, node) => bool

  let getWeight: (t<'a>, node, node) => option<'a>

  let adjacent: (t<'a>, node, node) => bool

  let neighbors: (t<'a>, node) => array<node>
}

module type T = {
  type t
}

module MakeImpl = (NodeC: StdlibFp.Map.S): (
  S with type node = NodeC.key and type t<'a> = NodeC.t<NodeC.key, NodeC.t<NodeC.key, option<'a>>>
) => {
  type node = NodeC.key
  //  type edgeContainer = EdgeC.t<NodeC.key>
  type t<'a> = NodeC.t<NodeC.key, NodeC.t<NodeC.key, option<'a>>>

  let make = NodeC.make

  let addNode = (t, node) => {
    switch t->NodeC.get(node) {
    | Some(_) => ()
    | None => t->NodeC.set(node, NodeC.make())
    }
  }

  let hasNode = (t, node) => t->NodeC.has(node)

  let adjacent = (t, a, b) => {
    switch t->NodeC.get(a) {
    | Some(ec) => ec->NodeC.has(b)
    | None => false
    }
  }

  let getAllNodes = t => t->NodeC.keys->Iterator.toArray

  let neighbors = (t, node) => {
    t->NodeC.get(node)->Option.getOr(NodeC.make())->NodeC.keys->Iterator.toArray
  }

  let removeNode = (t, node) => {
    let children = t->neighbors(node)
    children->Array.forEach(child => {
      switch t->NodeC.get(child) {
      | Some(c) => c->NodeC.delete(node)->ignore
      | None => ()
      }
    })
    t->NodeC.delete(node)
  }

  let addDirectedEdge = (t, a, b, ~weight: option<'a>=None) => {
    addNode(t, a)
    addNode(t, b)
    switch t->NodeC.get(a) {
    | Some(ec) => ec->NodeC.set(b, weight)
    | None => raise(Not_found)
    }
  }

  //  let addUndirectedEdge: (t<'a>, node, node, ~weight) => unit = (t, a, b) => {
  let addUndirectedEdge: (t<'a>, node, node, ~weight: option<'a>=?) => unit = (
    t,
    a,
    b,
    ~weight=None,
  ) => {
    addDirectedEdge(t, a, b, ~weight)
    addDirectedEdge(t, b, a, ~weight)
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

  let getWeight: (t<'a>, node, node) => option<'a> = (t, a, b) => {
    switch t->NodeC.get(a) {
    | Some(ec) => ec->NodeC.get(b)->Option.getOr(None)
    | None => None
    }
  }
}

module MakeWithPrimitive = (T: T): (S with type node = T.t) => {
  module NodeC = StdlibFp.Map.MakeWithPrimitive(T)
  include MakeImpl(NodeC)
}

module Make = (Serializable: Serializable.S): (S with type node = Serializable.t) => {
  module NodeC = StdlibFp.Map.Make(Serializable)
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
