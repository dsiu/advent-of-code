open Jest
open Expect

module StringAdjList = AdjacencyList.Node.String
module TupleAdjList = AdjacencyList.Node.Tuple2.StringInt

describe("Dijkstra Algorithm with StringAdjList", () => {
  module A = StringAdjList
  module NodeMap = unpack(A.nodeMap)
  module Dijkstra = Dijkstra.Dijkstra(A, NodeMap)

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

  let dijkstraTestData = list{
    ("1", Some(Some(0))),
    ("2", Some(Some(7))),
    ("3", Some(Some(9))),
    ("4", Some(Some(20))),
    ("5", Some(Some(26))),
    ("6", Some(Some(11))),
  }

  testAll("calculates correct shortest distance", dijkstraTestData, ((node, expected)) => {
    let (distances, _) = Dijkstra.dijkstra(graph.contents, "1")
    expect(distances->NodeMap.get(node))->toEqual(expected)
  })

  test("handles unreachable nodes", () => {
    graph.contents->A.addNode("7")
    let (distances, _) = Dijkstra.dijkstra(graph.contents, "1")
    expect(distances->NodeMap.get("7"))->toEqual(None)
  })

  test("returns correct path", () => {
    let path = Dijkstra.shortestPath(graph.contents, "1", "5")
    expect(path)->toEqual(Some(["1", "3", "4", "5"]))
  })

  test("returns None for unreachable nodes", () => {
    graph.contents->A.addNode("7")
    let path = Dijkstra.shortestPath(graph.contents, "1", "7")
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
    let paths = Dijkstra.shortestPaths(graph.contents, "1")
    expect(paths->NodeMap.get(node))->toEqual(expected)
  })
})

describe("Dijkstra Algorithm with TupleAdjList", () => {
  module A = TupleAdjList
  module NodeMap = unpack(A.nodeMap)
  module Dijkstra = Dijkstra.Dijkstra(A, NodeMap)

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

  let dijkstraTestData = list{
    (("1", 1), Some(Some(0))),
    (("2", 2), Some(Some(7))),
    (("3", 3), Some(Some(9))),
    (("4", 4), Some(Some(20))),
    (("5", 5), Some(Some(26))),
    (("6", 6), Some(Some(11))),
  }

  testAll("calculates correct shortest distance", dijkstraTestData, ((node, expected)) => {
    let (distances, _) = Dijkstra.dijkstra(graph.contents, ("1", 1))
    expect(distances->NodeMap.get(node))->toEqual(expected)
  })

  test("handles unreachable nodes", () => {
    graph.contents->A.addNode(("7", 7))
    let (distances, _) = Dijkstra.dijkstra(graph.contents, ("1", 1))
    expect(distances->NodeMap.get(("7", 7)))->toEqual(None)
  })

  test("returns correct path", () => {
    let path = Dijkstra.shortestPath(graph.contents, ("1", 1), ("5", 5))
    expect(path)->toEqual(Some([("1", 1), ("3", 3), ("4", 4), ("5", 5)]))
  })

  test("returns None for unreachable nodes", () => {
    graph.contents->A.addNode(("7", 7))
    let path = Dijkstra.shortestPath(graph.contents, ("1", 1), ("7", 7))
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
    let paths = Dijkstra.shortestPaths(graph.contents, ("1", 1))
    expect(paths->NodeMap.get(node))->toEqual(expected)
  })
})
