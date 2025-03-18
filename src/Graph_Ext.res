open StdlibFp

let log = Console.log
let log2 = Console.log2

//module type S { type t module Sub : sig type t val to_outer : t/1 -> t/2 end end

module A = AdjacencyList.Node.String

// Belt's MutableQueue seems to be quite performant
module Queue = Belt.MutableQueue
module Stack = Belt.MutableStack

module Traversal = {
  type traversalRecord<'a> = TraversalRecord({node: A.node, depth: int})
  type callback = (A.node, int) => bool // return true to stop the traversal

  let bfs: (A.t, A.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
    let queue = Queue.make()
    let rec loop = (graph, acc, visited, toVisit) => {
      toVisit->Queue.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth}) = toVisit->Queue.popExn
            visited->Set.has(node)
              ? loop(graph, acc, visited, toVisit) // tail recursion
              : {
                  acc->Array.push(TraversalRecord({node, depth}))
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
                              TraversalRecord({node: neighborNode, depth: depth + 1}),
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

    queue->Queue.add(TraversalRecord({node: rootNode, depth: 0}))
    loop(graph, [], Set.make(), queue)
  }

  let dfs: (A.t, A.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
    let stack = Stack.make()
    let rec loop = (graph, acc, visited, toVisit) => {
      toVisit->Stack.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth}) = toVisit->Stack.pop->Option.getExn
            visited->Set.has(node)
              ? loop(graph, acc, visited, toVisit) // tail recursion
              : {
                  acc->Array.push(TraversalRecord({node, depth}))
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
                              TraversalRecord({node: neighborNode, depth: depth + 1}),
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

    stack->Stack.push(TraversalRecord({node: rootNode, depth: 0}))
    loop(graph, [], Set.make(), stack)
  }
}

let _ = {
  let _ = {
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
    //  g->G.inspect->log

    "AdjacencyList BFS"->log
    let bfsRes = g->Traversal.bfs("1", (_node, _depth) => {
      //        log2(node, depth)

      // g->G.setNodeAttribute(node, "depth", depth)
      false
    })
    bfsRes->log2("bfsRes", _)

    //      g->GEXF.writeToFile("graph-bfs.gexf", module(G))
  }

  let _ = {
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
    //  g->G.inspect->log

    "AdjacencyList DFS"->log
    let dfsRes = g->Traversal.dfs("1", (_node, _depth) => {
      //        log2(node, depth)

      //        g->G.setNodeAttribute(node, "depth", depth)
      false
    })
    dfsRes->log2("dfsRes", _)

    //      g->GEXF.writeToFile("graph-dfs.gexf", module(G))
  }
}
