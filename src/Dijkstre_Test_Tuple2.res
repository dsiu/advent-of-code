let log = Console.log
let log2 = Console.log2

let _ = {
  module A = AdjacencyList.Node.Tuple2.IntInt
  module NodeMap = unpack(A.nodeC)
  module Dijkstra = Dijkstra.Dijkstra(A, NodeMap)

  log("Tuple2 as Node Test")
  let g = A.make()
  g->A.addDirectedEdge((1, 1), (2, 2), ~weight=Some(7))
  g->A.addDirectedEdge((1, 1), (3, 3), ~weight=Some(9))
  g->A.addDirectedEdge((1, 1), (6, 6), ~weight=Some(14))
  g->A.addDirectedEdge((2, 2), (3, 3), ~weight=Some(10))
  g->A.addDirectedEdge((2, 2), (4, 4), ~weight=Some(15))
  g->A.addDirectedEdge((3, 3), (4, 4), ~weight=Some(11))
  g->A.addDirectedEdge((3, 3), (6, 6), ~weight=Some(2))
  g->A.addDirectedEdge((4, 4), (5, 5), ~weight=Some(6))
  g->A.addDirectedEdge((5, 5), (6, 6), ~weight=Some(9))
  g->A.addNode((7, 7))

  g->log2("Graph: ", _)

  let (distances, previousNodes) = Dijkstra.dijkstra(g, (1, 1))

  log2("distances: ", distances)
  log2("previousNodes: ", previousNodes)

  switch distances->NodeMap.get((7, 7)) {
  | Some(x) => x->Console.log2("Distance from (1,1) to (7,7): ", _)
  | None => Console.log("Node (7,7) is unreachable from (1,1)")
  }

  distances
  ->NodeMap.entries
  ->Iterator.toArray
  ->Array.forEach(((node, distance)) => {
    switch distance {
    | Some(d) => Console.log4("Distance from 1 to", node, "is", d)
    | None => Console.log3("Node", node, "is unreachable from (1,1)")
    }
  })

  previousNodes
  ->NodeMap.entries
  ->Iterator.toArray
  ->Array.forEach(((node, pNode)) => {
    switch pNode {
    | Some(pn) => Console.log3(node, " has previous node ", pn)
    | None => Console.log3("Node", node, "does not have a previous node")
    }
  })

  let shortestPaths = Dijkstra.shortestPaths(g, (1, 1))
  shortestPaths->log2("All shortest paths from (1,1): ", _)

  let shortestPath = Dijkstra.shortestPath(g, (1, 1), (5, 5))
  shortestPath->log2("Shortest path from (1,1) to (5,5): ", _)

  let shortestPath17 = Dijkstra.shortestPath(g, (1, 1), (7, 7))
  shortestPath17->log2("Shortest path from (1,1) to (7,7): ", _)

  log("")
}
