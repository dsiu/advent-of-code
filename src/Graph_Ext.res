open StdlibFp

let log = Console.log
let log2 = Console.log2

//module type S { type t module Sub : sig type t val to_outer : t/1 -> t/2 end end

module A = AdjacencyList.Node.String

// Belt's MutableQueue seems to be quite performant
module Queue = Belt.MutableQueue
module Stack = Belt.MutableStack

module TraversalImpl = (A: AdjacencyList.S) => {
  type traversalRecord<'a> = TraversalRecord({node: A.node, depth: int, from: option<A.node>})
  type callback = (A.node, int) => bool // return true to stop the traversal

  let bfs: (A.t, A.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
    let queue = Queue.make()
    let rec loop = (graph, acc, visited, toVisit) => {
      toVisit->Queue.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth, from}) = toVisit->Queue.popExn
            visited->Set.has(node)
              ? loop(graph, acc, visited, toVisit) // tail recursion
              : {
                  acc->Array.push(TraversalRecord({node, depth, from}))
                  visited->Set.add(node)

                  !cb(node, depth)
                    ? {
                        graph
                        ->A.neighbors(node)
                        ->Array.valuesIter
                        ->Iterator.forEach(neighbor => {
                          switch neighbor {
                          | Some(neighborNode) =>
                            queue->Queue.add(
                              TraversalRecord({
                                node: neighborNode,
                                depth: depth + 1,
                                from: Some(node),
                              }),
                            )
                          | None => ()
                          }
                        })
                      }
                    : ()
                  // tail recursion
                  loop(graph, acc, visited, toVisit)
                }
          }
    }

    queue->Queue.add(TraversalRecord({node: rootNode, depth: 0, from: None}))
    loop(graph, [], Set.make(), queue)
  }

  let dfs: (A.t, A.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
    let stack = Stack.make()
    let rec loop = (graph, acc, visited, toVisit) => {
      toVisit->Stack.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth, from}) = toVisit->Stack.pop->Option.getExn
            visited->Set.has(node)
              ? loop(graph, acc, visited, toVisit) // tail recursion
              : {
                  acc->Array.push(TraversalRecord({node, depth, from}))
                  visited->Set.add(node)

                  !cb(node, depth)
                    ? {
                        graph
                        ->A.neighbors(node)
                        ->Array.valuesIter
                        ->Iterator.forEach(neighbor => {
                          switch neighbor {
                          | Some(neighborNode) =>
                            stack->Stack.push(
                              TraversalRecord({
                                node: neighborNode,
                                depth: depth + 1,
                                from: Some(node),
                              }),
                            )
                          | None => ()
                          }
                        })
                      }
                    : ()
                  // tail recursion
                  loop(graph, acc, visited, toVisit)
                }
          }
    }

    stack->Stack.push(TraversalRecord({node: rootNode, depth: 0, from: None}))
    loop(graph, [], Set.make(), stack)
  }

  let convertToPaths = (records: array<traversalRecord<'a>>): array<array<A.node>> => {
    let rec buildPath = (record: traversalRecord<'a>, acc: array<A.node>): array<A.node> => {
      let TraversalRecord({node, depth, from}) = record
      switch from {
      | None => [node, ...acc]
      | Some(fromNode) =>
        let fromRecord =
          records->Array.find((TraversalRecord({node, _})) => node == fromNode)->Option.getExn
        buildPath(fromRecord, [node, ...acc])
      }
    }

    records->Array.map(record => buildPath(record, []))
  }
}

module Make = (A: AdjacencyList.S) => {
  module Traversal = TraversalImpl(A)
}

let _ = {
  let _ = {
    log("AdjacencyList - String")
    module A = AdjacencyList.Node.String
    module G = Make(A)
    module Traversal = G.Traversal
    let g = A.make()
    let _ = g->A.addDirectedEdge("1", "2")
    let _ = g->A.addDirectedEdge("1", "3")
    let _ = g->A.addDirectedEdge("1", "4")
    let _ = g->A.addDirectedEdge("2", "5")
    let _ = g->A.addDirectedEdge("2", "6")
    let _ = g->A.addDirectedEdge("4", "7")
    let _ = g->A.addDirectedEdge("4", "8")
    let _ = g->A.addDirectedEdge("5", "9")
    let _ = g->A.addDirectedEdge("5", "10")
    let _ = g->A.addDirectedEdge("7", "11")
    let _ = g->A.addDirectedEdge("7", "12")
    //    g->log2(g)

    "AdjacencyList BFS"->log
    let bfsRes = g->Traversal.bfs("1", (_node, _depth) => {
      // log2(node, depth)
      // g->G.setNodeAttribute(node, "depth", depth)
      false
    })
    bfsRes->log2("bfsRes", _)

    let bfsPaths = bfsRes->G.Traversal.convertToPaths
    bfsPaths->log2("bfsPaths", _)

    "AdjacencyList DFS"->log
    let dfsRes = g->Traversal.dfs("1", (_node, _depth) => {
      //        log2(node, depth)
      //        g->G.setNodeAttribute(node, "depth", depth)
      false
    })
    dfsRes->log2("dfsRes", _)

    let dfsPaths = dfsRes->G.Traversal.convertToPaths
    dfsPaths->log2("dfsPaths", _)
  }

  let _ = {
    log("AdjacencyList - (int, int)")

    module A = AdjacencyList.Node.Tuple2.IntInt
    module G = Make(A)
    module Traversal = G.Traversal
    let g = A.make()

    let _ = g->A.addDirectedEdge((1, 2), (2, 3))
    let _ = g->A.addDirectedEdge((1, 2), (3, 4))
    let _ = g->A.addDirectedEdge((1, 2), (4, 5))
    let _ = g->A.addDirectedEdge((2, 3), (5, 6))
    let _ = g->A.addDirectedEdge((2, 3), (6, 7))
    let _ = g->A.addDirectedEdge((4, 5), (7, 8))
    let _ = g->A.addDirectedEdge((4, 5), (8, 9))
    let _ = g->A.addDirectedEdge((5, 6), (9, 10))
    let _ = g->A.addDirectedEdge((5, 6), (10, 11))
    let _ = g->A.addDirectedEdge((7, 8), (11, 12))
    let _ = g->A.addDirectedEdge((7, 8), (12, 13))
    //    g->log2(g)

    "AdjacencyList DFS"->log
    let dfsRes = g->Traversal.dfs((1, 2), (_node, _depth) => {
      //        log2(node, depth)
      //        g->G.setNodeAttribute(node, "depth", depth)
      false
    })
    dfsRes->log2("dfsRes", _)

    let dfsPaths = dfsRes->G.Traversal.convertToPaths
    dfsPaths->log2("dfsPaths", _)
  }
}
