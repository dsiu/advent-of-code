open Jest
open Expect

module StringAdjList = AdjacencyList.Node.String
module TupleAdjList = AdjacencyList.Node.Tuple2.StringInt

describe("Graph_Ext with StringAdjList", () => {
  module A = StringAdjList
  module NodeSet = unpack(A.nodeSet)
  module Traversal = Graph_Ext.Traversal(A, NodeSet)

  let graph = ref(A.make())

  beforeEach(() => {
    graph := A.make()
    graph.contents->A.addDirectedEdge("1", "2", ~weight=Some(7))
    graph.contents->A.addDirectedEdge("1", "3", ~weight=Some(9))
    graph.contents->A.addDirectedEdge("1", "6", ~weight=Some(14))
    graph.contents->A.addDirectedEdge("2", "3", ~weight=Some(10))
    graph.contents->A.addDirectedEdge("2", "4", ~weight=Some(15))
    graph.contents->A.addDirectedEdge("3", "4", ~weight=Some(11))
    graph.contents->A.addDirectedEdge("3", "6", ~weight=Some(2))
    graph.contents->A.addDirectedEdge("4", "5", ~weight=Some(6))
    graph.contents->A.addDirectedEdge("5", "6", ~weight=Some(9))
  })

  test("BFS traversal", () => {
    let bfsResult = Traversal.bfs("1", A.neighbors(graph.contents, _), (_node, _distance) => false)
    let expected = [
      Traversal.TraversalRecord({node: "1", depth: 0, from: None}),
      Traversal.TraversalRecord({node: "2", depth: 1, from: Some("1")}),
      Traversal.TraversalRecord({node: "3", depth: 1, from: Some("1")}),
      Traversal.TraversalRecord({node: "6", depth: 1, from: Some("1")}),
      Traversal.TraversalRecord({node: "4", depth: 2, from: Some("2")}),
      Traversal.TraversalRecord({node: "5", depth: 3, from: Some("4")}),
    ]
    expect(bfsResult)->toEqual(expected)
  })

  test("BFS all paths", () => {
    let bfsResult = Traversal.bfs("1", A.neighbors(graph.contents, _), (_node, _distance) => false)
    let paths = bfsResult->Traversal.paths
    let expected = [
      ["1"],
      ["1", "2"],
      ["1", "3"],
      ["1", "6"],
      ["1", "2", "4"],
      ["1", "2", "4", "5"],
    ]
    expect(paths)->toEqual(expected)
  })

  test("BFS path between nodes", () => {
    let bfsResult = Traversal.bfs("1", A.neighbors(graph.contents, _), (_node, _distance) => false)
    let path = Traversal.path(bfsResult, "1", "5")
    let expected = Some(["1", "2", "4", "5"])
    expect(path)->toEqual(expected)
  })

  test("DFS traversal", () => {
    let dfsResult = Traversal.dfs("1", A.neighbors(graph.contents, _), (_node, _distance) => false)
    let expected = [
      Traversal.TraversalRecord({node: "1", depth: 0, from: None}),
      Traversal.TraversalRecord({node: "6", depth: 1, from: Some("1")}),
      Traversal.TraversalRecord({node: "3", depth: 1, from: Some("1")}),
      Traversal.TraversalRecord({node: "4", depth: 2, from: Some("3")}),
      Traversal.TraversalRecord({node: "5", depth: 3, from: Some("4")}),
      Traversal.TraversalRecord({node: "2", depth: 1, from: Some("1")}),
    ]
    expect(dfsResult)->toEqual(expected)
  })

  test("DFS all paths", () => {
    let dfsResult = Traversal.dfs("1", A.neighbors(graph.contents, _), (_node, _distance) => false)
    let paths = dfsResult->Traversal.paths
    let expected = [
      ["1"],
      ["1", "6"],
      ["1", "3"],
      ["1", "3", "4"],
      ["1", "3", "4", "5"],
      ["1", "2"],
    ]
    expect(paths)->toEqual(expected)
  })

  test("DFS path between nodes", () => {
    let dfsResult = Traversal.dfs("1", A.neighbors(graph.contents, _), (_node, _distance) => false)
    let path = Traversal.path(dfsResult, "1", "5")
    let expected = Some(["1", "3", "4", "5"])
    expect(path)->toEqual(expected)
  })
})

describe("Graph_Ext with TupleAdjList", () => {
  module A = TupleAdjList
  module NodeSet = unpack(A.nodeSet)

  module Traversal = Graph_Ext.Traversal(A, NodeSet)

  let graph = ref(A.make())

  beforeEach(() => {
    graph := A.make()
    graph.contents->A.addDirectedEdge(("1", 1), ("2", 2), ~weight=Some(7))
    graph.contents->A.addDirectedEdge(("1", 1), ("3", 3), ~weight=Some(9))
    graph.contents->A.addDirectedEdge(("1", 1), ("6", 6), ~weight=Some(14))
    graph.contents->A.addDirectedEdge(("2", 2), ("3", 3), ~weight=Some(10))
    graph.contents->A.addDirectedEdge(("2", 2), ("4", 4), ~weight=Some(15))
    graph.contents->A.addDirectedEdge(("3", 3), ("4", 4), ~weight=Some(11))
    graph.contents->A.addDirectedEdge(("3", 3), ("6", 6), ~weight=Some(2))
    graph.contents->A.addDirectedEdge(("4", 4), ("5", 5), ~weight=Some(6))
    graph.contents->A.addDirectedEdge(("5", 5), ("6", 6), ~weight=Some(9))
  })

  test("BFS traversal", () => {
    let bfsResult = Traversal.bfs(
      ("1", 1),
      A.neighbors(graph.contents, _),
      (_node, _distance) => false,
    )

    let expected = [
      Traversal.TraversalRecord({node: ("1", 1), depth: 0, from: None}),
      Traversal.TraversalRecord({node: ("2", 2), depth: 1, from: Some(("1", 1))}),
      Traversal.TraversalRecord({node: ("3", 3), depth: 1, from: Some(("1", 1))}),
      Traversal.TraversalRecord({node: ("6", 6), depth: 1, from: Some(("1", 1))}),
      Traversal.TraversalRecord({node: ("4", 4), depth: 2, from: Some(("2", 2))}),
      Traversal.TraversalRecord({node: ("5", 5), depth: 3, from: Some(("4", 4))}),
    ]

    expect(bfsResult)->toEqual(expected)
  })

  test("BFS all paths", () => {
    let bfsResult = Traversal.bfs(
      ("1", 1),
      A.neighbors(graph.contents, _),
      (_node, _distance) => false,
    )
    let paths = bfsResult->Traversal.paths
    let expected = [
      [("1", 1)],
      [("1", 1), ("2", 2)],
      [("1", 1), ("3", 3)],
      [("1", 1), ("6", 6)],
      [("1", 1), ("2", 2), ("4", 4)],
      [("1", 1), ("2", 2), ("4", 4), ("5", 5)],
    ]
    expect(paths)->toEqual(expected)
  })

  test("BFS path between nodes", () => {
    let bfsResult = Traversal.bfs(
      ("1", 1),
      A.neighbors(graph.contents, _),
      (_node, _distance) => false,
    )
    let path = Traversal.path(bfsResult, ("1", 1), ("5", 5))
    let expected = Some([("1", 1), ("2", 2), ("4", 4), ("5", 5)])
    expect(path)->toEqual(expected)
  })

  test("DFS traversal", () => {
    let dfsResult = Traversal.dfs(
      ("1", 1),
      A.neighbors(graph.contents, _),
      (_node, _distance) => false,
    )
    let expected = [
      Traversal.TraversalRecord({node: ("1", 1), depth: 0, from: None}),
      Traversal.TraversalRecord({node: ("6", 6), depth: 1, from: Some(("1", 1))}),
      Traversal.TraversalRecord({node: ("3", 3), depth: 1, from: Some(("1", 1))}),
      Traversal.TraversalRecord({node: ("4", 4), depth: 2, from: Some(("3", 3))}),
      Traversal.TraversalRecord({node: ("5", 5), depth: 3, from: Some(("4", 4))}),
      Traversal.TraversalRecord({node: ("2", 2), depth: 1, from: Some(("1", 1))}),
    ]
    expect(dfsResult)->toEqual(expected)
  })

  test("DFS all paths", () => {
    let dfsResult = Traversal.dfs(
      ("1", 1),
      A.neighbors(graph.contents, _),
      (_node, _distance) => false,
    )
    let paths = dfsResult->Traversal.paths
    let expected = [
      [("1", 1)],
      [("1", 1), ("6", 6)],
      [("1", 1), ("3", 3)],
      [("1", 1), ("3", 3), ("4", 4)],
      [("1", 1), ("3", 3), ("4", 4), ("5", 5)],
      [("1", 1), ("2", 2)],
    ]
    expect(paths)->toEqual(expected)
  })

  test("DFS path between nodes", () => {
    let dfsResult = Traversal.dfs(
      ("1", 1),
      A.neighbors(graph.contents, _),
      (_node, _distance) => false,
    )
    let path = Traversal.path(dfsResult, ("1", 1), ("5", 5))
    let expected = Some([("1", 1), ("3", 3), ("4", 4), ("5", 5)])
    expect(path)->toEqual(expected)
  })
})
