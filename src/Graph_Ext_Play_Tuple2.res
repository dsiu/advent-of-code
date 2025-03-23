let log = Console.log
let log2 = Console.log2

let _ = {
  module TupleAdjList = AdjacencyList.Node.Tuple2.StringInt

  module A = TupleAdjList
  module NodeSet = unpack(A.nodeSet)
  module Traversal = Graph_Ext.Traversal(A, NodeSet)
  let graph = A.make()
  graph->A.addDirectedEdge(("1", 1), ("2", 2), ~weight=Some(7))
  graph->A.addDirectedEdge(("1", 1), ("3", 3), ~weight=Some(9))
  graph->A.addDirectedEdge(("1", 1), ("6", 6), ~weight=Some(14))
  graph->A.addDirectedEdge(("2", 2), ("3", 3), ~weight=Some(10))
  graph->A.addDirectedEdge(("2", 2), ("4", 4), ~weight=Some(15))
  graph->A.addDirectedEdge(("3", 3), ("4", 4), ~weight=Some(11))
  graph->A.addDirectedEdge(("3", 3), ("6", 6), ~weight=Some(2))
  graph->A.addDirectedEdge(("4", 4), ("5", 5), ~weight=Some(6))
  graph->A.addDirectedEdge(("5", 5), ("6", 6), ~weight=Some(9))

  let bfsResult = graph->Traversal.bfs(("1", 1), (_node, _distance) => false)

  bfsResult->Console.log2("BFS Tuple2 traversal:", _)
}
