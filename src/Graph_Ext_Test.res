let log = Console.log
let log2 = Console.log2

let _ = {
  let _ = {
    log("AdjacencyList - String")
    module A = AdjacencyList.Node.String
    module G = Graph_Ext.Make(A)
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
    module G = Graph_Ext.Make(A)
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

  let _ = {
    log("AdjacencyList - (int, int) with weight")

    module A = AdjacencyList.Node.Tuple2.IntInt
    module G = Graph_Ext.Make(A)
    module Traversal = G.Traversal
    let g = A.make()

    let _ = g->A.addDirectedEdge((1, 2), (2, 3), ~weight=Some(8))
    let _ = g->A.addDirectedEdge((1, 2), (3, 4), ~weight=Some(10))
    let _ = g->A.addDirectedEdge((1, 2), (4, 5), ~weight=Some(12))
    let _ = g->A.addDirectedEdge((2, 3), (5, 6), ~weight=Some(14))
    let _ = g->A.addDirectedEdge((2, 3), (6, 7), ~weight=Some(16))
    let _ = g->A.addDirectedEdge((4, 5), (7, 8), ~weight=Some(18))
    let _ = g->A.addDirectedEdge((4, 5), (8, 9), ~weight=Some(20))
    let _ = g->A.addDirectedEdge((5, 6), (9, 10), ~weight=Some(22))
    let _ = g->A.addDirectedEdge((5, 6), (10, 11), ~weight=Some(24))
    let _ = g->A.addDirectedEdge((7, 8), (11, 12), ~weight=Some(26))
    let _ = g->A.addDirectedEdge((7, 8), (12, 13), ~weight=Some(28))
    g->log2(g)

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
