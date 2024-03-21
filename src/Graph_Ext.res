open Stdlib
open Graphology

let log = Console.log
let log2 = Console.log2
type node = string
type edge = string

// Belt's MutableQueue seems to be quite performant
module Queue = Belt.MutableQueue
module Stack = Belt.MutableStack
module Set = Belt.MutableSet.String

module G = Graph.MakeGraph({
  type node = node
  type edge = edge
})

// todo: refactor
let stringToFile = (str, ~fileName) => {
  open NodeJs
  Fs.writeFileSync(fileName, str->Buffer.fromString)
}

module GEXF = {
  let writeToFile = (g, filename) => {
    let gexfStrWithOptions = g->G.GEXF.write(
      ~options={
        version: "1.3",
        formatNode: (key, _attributes) => {
          {
            "label": key,
            "attributes": {
              "name": key,
            },
          }
        },
        formatEdge: (key, _attributes) => {
          {
            "label": key,
            "attributes": {
              "name": key,
            },
          }
        },
      },
    )

    gexfStrWithOptions->stringToFile(~fileName=filename)
  }
}

module Traversal = {
  type traversalRecord<'a> = TraversalRecord({node: G.node, depth: int})
  type callback = (G.node, int) => bool // return true to stop the traversal

  let bfs: (G.t, G.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
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
                        graph->G.NeighborsIter.forEachOutboundNeighbor(
                          Node(
                            node,
                            (neighbor, _neighborAttr) => {
                              queue->Queue.add(TraversalRecord({node: neighbor, depth: depth + 1}))
                            },
                          ),
                        )
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

  let dfs: (G.t, G.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
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
                        graph->G.NeighborsIter.forEachOutboundNeighbor(
                          Node(
                            node,
                            (neighbor, _neighborAttr) => {
                              stack->Stack.push(TraversalRecord({node: neighbor, depth: depth + 1}))
                            },
                          ),
                        )
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
  let g = G.makeGraph()
  let _ = g->G.mergeEdge("1", "2")
  let _ = g->G.mergeEdge("1", "3")
  let _ = g->G.mergeEdge("1", "4")
  let _ = g->G.mergeEdge("2", "5")
  let _ = g->G.mergeEdge("2", "6")
  let _ = g->G.mergeEdge("4", "7")
  let _ = g->G.mergeEdge("4", "8")
  let _ = g->G.mergeEdge("5", "9")
  let _ = g->G.mergeEdge("5", "10")
  let _ = g->G.mergeEdge("7", "11")
  let _ = g->G.mergeEdge("7", "12")
  g->G.inspect->log

  "BFS"->log
  let bfsRes = g->Traversal.bfs("1", (node, depth) => {
    log2(node, depth)
    false
  })
  bfsRes->log2("bfsRes")

  "DFS"->log
  let dfsRes = g->Traversal.dfs("1", (node, depth) => {
    log2(node, depth)
    false
  })
  dfsRes->log2("dfsRes")

  //  g->GEXF.writeToFile("graph.gexf")
}
