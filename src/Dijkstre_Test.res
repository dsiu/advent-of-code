let log = Console.log
let log2 = Console.log2

// Example usage
let _ = {
  module A = AdjacencyList.Node.String
  module NodeMap = unpack(A.nodeC)
  module Dijkstra = Dijkstra.Dijkstra(A, NodeMap)

  log("String as Node Test")
  let g = A.make()
  g->A.addDirectedEdge("1", "2", ~weight=Some(7))
  g->A.addDirectedEdge("1", "3", ~weight=Some(9))
  g->A.addDirectedEdge("1", "6", ~weight=Some(14))
  g->A.addDirectedEdge("2", "3", ~weight=Some(10))
  g->A.addDirectedEdge("2", "4", ~weight=Some(15))
  g->A.addDirectedEdge("3", "4", ~weight=Some(11))
  g->A.addDirectedEdge("3", "6", ~weight=Some(2))
  g->A.addDirectedEdge("4", "5", ~weight=Some(6))
  g->A.addDirectedEdge("5", "6", ~weight=Some(9))

  g->log2("Graph: ", _)

  let (distances, previousNodes) = Dijkstra.dijkstra(g, "1")

  log2("distances: ", distances)
  log2("previousNodes: ", previousNodes)

  distances
  ->NodeMap.entries
  ->Iterator.toArray
  ->Array.forEach(((node, distance)) => {
    switch distance {
    | Some(d) => Console.log4("Distance from 1 to", node, "is", d)
    | None => Console.log3("Node", node, "is unreachable from 1")
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

  let shortestPaths = Dijkstra.shortestPaths(g, "1")
  shortestPaths->log2("Shortest paths from 1: ", _)

  let shortestPath = Dijkstra.shortestPath(g, "1", "5")
  shortestPath->log2("Shortest path from 1 to 5: ", _)

  log("")
}

//let _ = {
//  module A = AdjacencyList.Node.Float
//  module G = Graph_Ext.Make(A)
//  module Dijkstra = Dijkstra.Dijkstra(A)
//
//  log("Float as Node Test")
//  let g = A.make()
//  g->A.addDirectedEdge(1., 2., ~weight=Some(7))
//  g->A.addDirectedEdge(1., 3., ~weight=Some(9))
//  g->A.addDirectedEdge(1., 6., ~weight=Some(14))
//  g->A.addDirectedEdge(2., 3., ~weight=Some(10))
//  g->A.addDirectedEdge(2., 4., ~weight=Some(15))
//  g->A.addDirectedEdge(3., 4., ~weight=Some(11))
//  g->A.addDirectedEdge(3., 6., ~weight=Some(2))
//  g->A.addDirectedEdge(4., 5., ~weight=Some(6))
//  g->A.addDirectedEdge(5., 6., ~weight=Some(9))
//
//  let (distances, previousNodes) = Dijkstra.dijkstra(g, 1.)
//  distances
//  ->Map.entries
//  ->Iterator.toArray
//  ->Array.forEach(((node, distance)) => {
//    switch distance {
//    | Some(d) => Console.log4("Distance from 1 to", node, "is", d)
//    | None => Console.log3("Node", node, "is unreachable from 1")
//    }
//  })
//
//  previousNodes
//  ->Map.entries
//  ->Iterator.toArray
//  ->Array.forEach(((node, pNode)) => {
//    switch pNode {
//    | Some(pn) => Console.log3(node, " has previous node ", pn)
//    | None => Console.log3("Node", node, "does not have a previous node")
//    }
//  })
//
//  let shortestPaths = Dijkstra.shortestPaths(g, 1.)
//  shortestPaths->log2("All shortest paths from 1.: ", _)
//
//  let shortestPath = Dijkstra.shortestPath(g, 1., 5.)
//  shortestPath->log2("Shortest path from 1. to 5.: ", _)
//
//  log("")
//}
