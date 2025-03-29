let log = Console.log
let log2 = Console.log2

let _ = {
  module StringAdjList = AdjacencyList.Node.String

  module A = StringAdjList
  module NodeSet = unpack(A.nodeSet)
  module NodeMap = unpack(A.nodeMap)
  module AStar = AStar.AStar(A, NodeMap, NodeSet)

  let graph = A.make()

  graph->A.addDirectedEdge("1", "2", ~weight=Some(7))
  graph->A.addDirectedEdge("1", "3", ~weight=Some(9))
  graph->A.addDirectedEdge("1", "6", ~weight=Some(14))
  graph->A.addDirectedEdge("2", "3", ~weight=Some(10))
  graph->A.addDirectedEdge("2", "4", ~weight=Some(15))
  graph->A.addDirectedEdge("3", "4", ~weight=Some(11))
  graph->A.addDirectedEdge("3", "6", ~weight=Some(2))
  graph->A.addDirectedEdge("4", "5", ~weight=Some(6))
  graph->A.addDirectedEdge("5", "6", ~weight=Some(9))

  let start = "1"
  let goal = "5"

  let heuristic = (a, b) => {
    0.0
  }

  let cost = (a, b) => {
    switch graph->A.getWeight(a, b) {
    | Some(weight) => Float.fromInt(weight)
    | None => raise(Not_found)
    }
  }

  let neighbors = node => {
    graph->A.neighbors(node)
  }

  let path = AStar.aStar(start, goal, neighbors, cost, heuristic)

  log2("Path: ", path)
}
