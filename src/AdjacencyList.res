module Serializable = StdlibFp.Serializable
module JSONSerializable = StdlibFp.JSONSerializable

module type S = {
  type node

  type t<'a>

  let make: unit => t<'a>

  let nodeMap: module(StdlibFp.Map.S with type key = node)
  let nodeSet: module(StdlibFp.Set.S with type a = node)

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

module MakeImpl = (NodeMap: StdlibFp.Map.S, NodeSet: StdlibFp.Set.S with type a = NodeMap.key): (
  S
    with type node = NodeMap.key
    and type t<'a> = NodeMap.t<NodeMap.key, NodeMap.t<NodeMap.key, option<'a>>>
) => {
  type node = NodeMap.key
  //  type edgeContainer = EdgeC.t<NodeMap.key>
  type t<'a> = NodeMap.t<NodeMap.key, NodeMap.t<NodeMap.key, option<'a>>>

  let nodeMap = module(NodeMap: StdlibFp.Map.S with type key = node)
  let nodeSet = module(NodeSet: StdlibFp.Set.S with type a = node)

  let make = NodeMap.make

  let addNode = (t, node) => {
    switch t->NodeMap.get(node) {
    | Some(_) => ()
    | None => t->NodeMap.set(node, NodeMap.make())
    }
  }

  let hasNode = (t, node) => t->NodeMap.has(node)

  let adjacent = (t, a, b) => {
    switch t->NodeMap.get(a) {
    | Some(ec) => ec->NodeMap.has(b)
    | None => false
    }
  }

  let getAllNodes = t => t->NodeMap.keys->Iterator.toArray

  let neighbors = (t, node) => {
    t->NodeMap.get(node)->Option.getOr(NodeMap.make())->NodeMap.keys->Iterator.toArray
  }

  let removeNode = (t, node) => {
    let children = t->neighbors(node)
    children->Array.forEach(child => {
      switch t->NodeMap.get(child) {
      | Some(c) => c->NodeMap.delete(node)->ignore
      | None => ()
      }
    })
    t->NodeMap.delete(node)
  }

  let addDirectedEdge = (t, a, b, ~weight: option<'a>=None) => {
    addNode(t, a)
    addNode(t, b)
    switch t->NodeMap.get(a) {
    | Some(ec) => ec->NodeMap.set(b, weight)
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
    switch t->NodeMap.get(a) {
    | Some(ec) => ec->NodeMap.delete(b)
    | None => false
    } &&
    switch t->NodeMap.get(b) {
    | Some(ec) => ec->NodeMap.delete(a)
    | None => false
    }
  }

  let removeDirectedEdge = (t, a, b) => {
    switch t->NodeMap.get(a) {
    | Some(ec) => ec->NodeMap.delete(b)
    | None => false
    }
  }

  let getWeight: (t<'a>, node, node) => option<'a> = (t, a, b) => {
    switch t->NodeMap.get(a) {
    | Some(ec) => ec->NodeMap.get(b)->Option.getOr(None)
    | None => None
    }
  }
}

module MakeWithPrimitive = (T: T): (S with type node = T.t) => {
  module NodeMap = StdlibFp.Map.MakeWithPrimitive(T)
  module NodeSet = StdlibFp.Set.MakeWithPrimitive(T)
  include MakeImpl(NodeMap, NodeSet)
}

module Make = (Serializable: Serializable.S): (S with type node = Serializable.t) => {
  module NodeMap = StdlibFp.Map.Make(Serializable)
  module NodeSet = StdlibFp.Set.Make(Serializable)
  include MakeImpl(NodeMap, NodeSet)
}

module Node = {
  module String = MakeWithPrimitive({
    type t = string
  })

  module Int = MakeWithPrimitive({
    type t = int
  })

  module Float = MakeWithPrimitive({
    type t = float
  })

  module BigInt = MakeWithPrimitive({
    type t = bigint
  })

  module Tuple2 = {
    module Make = (A: JSONSerializable.S, B: JSONSerializable.S) => Make(
      Serializable.MakeTuple2(A, B),
    )
    module IntInt = Make(JSONSerializable.Int, JSONSerializable.Int)
    module StringInt = Make(JSONSerializable.String, JSONSerializable.Int)
  }
}

module Import = {
  module A = Node.Tuple2.IntInt

  let fromArray2D = (array2D: Array2D.t<'a>): A.t<'a> => {
    let addEdges = (adjList, x, y) => {
      let currentNode = (x, y)
      adjList->A.addNode(currentNode)

      let neighbors = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

      neighbors
      ->Array.filter(((nx, ny)) => array2D->Array2D.isValidXY((nx, ny)))
      ->Array.forEach(((nx, ny)) => {
        let neighborNode = (nx, ny)
        adjList->A.addNode(neighborNode)
        adjList->A.addDirectedEdge(currentNode, neighborNode)
      })
    }

    array2D->Array2D.reduceWithIndex(A.make(), (acc, _, (x, y)) => {
      acc->addEdges(x, y)
      acc
    })
  }
}
