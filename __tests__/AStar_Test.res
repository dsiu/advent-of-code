let log = Console.log
let log2 = Console.log2

open Jest
open Expect

module StringAdjList = AdjacencyList.Node.String
module TupleAdjList = AdjacencyList.Node.Tuple2.StringInt

describe("AStar Algorithm with string", () => {
  module A = StringAdjList
  module NodeMap = unpack(A.nodeMap)
  module NodeSet = unpack(A.nodeSet)
  module AStar = AStar.AStar(A, NodeMap, NodeSet)

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

  let heuristic = (a, b) => 0.0

  let cost = (a, b) => {
    switch graph.contents->A.getWeight(a, b) {
    | Some(weight) => Float.fromInt(weight)
    | None => raise(Not_found)
    }
  }

  let neighbors = node => {
    graph.contents->A.neighbors(node)
  }

  test("handles unreachable nodes", () => {
    graph.contents->A.addNode("7")
    let path = AStar.aStar("1", "7", neighbors, cost, heuristic)
    expect(path)->toEqual(None)
  })

  test("returns correct path", () => {
    let path = AStar.aStar("1", "5", neighbors, cost, heuristic)
    expect(path)->toEqual(Some(["1", "3", "4", "5"]))
  })

  test("returns None for unreachable nodes", () => {
    graph.contents->A.addNode("7")
    let path = AStar.aStar("1", "7", neighbors, cost, heuristic)
    expect(path)->toEqual(None)
  })

  let shortestPathsTestData = list{
    ("1", Some(["1"])),
    ("2", Some(["1", "2"])),
    ("3", Some(["1", "3"])),
    ("4", Some(["1", "3", "4"])),
    ("5", Some(["1", "3", "4", "5"])),
    ("6", Some(["1", "3", "6"])),
  }

  testAll("returns correct paths for all nodes", shortestPathsTestData, ((node, expected)) => {
    let paths = AStar.aStar("1", node, neighbors, cost, heuristic)
    expect(paths)->toEqual(expected)
  })
})

describe("AStar Algorithm with Tuple2 (string,int)", () => {
  module A = TupleAdjList
  module NodeMap = unpack(A.nodeMap)
  module NodeSet = unpack(A.nodeSet)
  module AStar = AStar.AStar(A, NodeMap, NodeSet)

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

  let heuristic = (a, b) => 0.0

  let cost = (a, b) => {
    switch graph.contents->A.getWeight(a, b) {
    | Some(weight) => Float.fromInt(weight)
    | None => raise(Not_found)
    }
  }

  let neighbors = node => {
    graph.contents->A.neighbors(node)
  }

  test("handles unreachable nodes", () => {
    graph.contents->A.addNode(("7", 7))
    let path = AStar.aStar(("1", 1), ("7", 7), neighbors, cost, heuristic)
    expect(path)->toEqual(None)
  })

  test("returns correct path", () => {
    let path = AStar.aStar(("1", 1), ("5", 5), neighbors, cost, heuristic)
    expect(path)->toEqual(Some([("1", 1), ("3", 3), ("4", 4), ("5", 5)]))
  })

  test("returns None for unreachable nodes", () => {
    graph.contents->A.addNode(("7", 7))
    let path = AStar.aStar(("1", 1), ("7", 7), neighbors, cost, heuristic)
    expect(path)->toEqual(None)
  })

  let shortestPathsTestData = list{
    (("1", 1), Some([("1", 1)])),
    (("2", 2), Some([("1", 1), ("2", 2)])),
    (("3", 3), Some([("1", 1), ("3", 3)])),
    (("4", 4), Some([("1", 1), ("3", 3), ("4", 4)])),
    (("5", 5), Some([("1", 1), ("3", 3), ("4", 4), ("5", 5)])),
    (("6", 6), Some([("1", 1), ("3", 3), ("6", 6)])),
  }

  testAll("returns correct paths for all nodes", shortestPathsTestData, ((node, expected)) => {
    let paths = AStar.aStar(("1", 1), node, neighbors, cost, heuristic)
    expect(paths)->toEqual(expected)
  })
})
