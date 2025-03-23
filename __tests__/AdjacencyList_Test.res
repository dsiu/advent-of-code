open Jest
open Expect

module StringAdjList = AdjacencyList.Node.String
module TupleAdjList = AdjacencyList.Node.Tuple2.StringInt

describe("AdjacencyList String Implementation", () => {
  let graph = ref(StringAdjList.make())

  beforeEach(() => {
    graph := StringAdjList.make()
    StringAdjList.addNode(graph.contents, "A")
    StringAdjList.addNode(graph.contents, "B")
    StringAdjList.addNode(graph.contents, "C")
    StringAdjList.addNode(graph.contents, "D")
    StringAdjList.addNode(graph.contents, "E")
    StringAdjList.addDirectedEdge(graph.contents, "A", "B", ~weight=Some(1.0))
    StringAdjList.addDirectedEdge(graph.contents, "B", "C", ~weight=Some(2.0))
    StringAdjList.addDirectedEdge(graph.contents, "C", "D", ~weight=Some(3.0))
    StringAdjList.addDirectedEdge(graph.contents, "D", "E", ~weight=Some(4.0))
    StringAdjList.addDirectedEdge(graph.contents, "E", "A", ~weight=Some(5.0))
    StringAdjList.addDirectedEdge(graph.contents, "A", "C", ~weight=Some(6.0))
    StringAdjList.addDirectedEdge(graph.contents, "B", "D", ~weight=Some(7.0))
  })

  test("get all nodes", () => {
    let nodes = StringAdjList.getAllNodes(graph.contents)
    expect(nodes)->toEqual(["A", "B", "C", "D", "E"])
  })

  test("add node", () => {
    StringAdjList.addNode(graph.contents, "F")
    expect(StringAdjList.hasNode(graph.contents, "F"))->toEqual(true)
  })

  test("add edge", () => {
    expect(StringAdjList.adjacent(graph.contents, "A", "B"))->toBe(true)
  })

  test("remove edge", () => {
    StringAdjList.removeDirectedEdge(graph.contents, "A", "B")->ignore
    expect(StringAdjList.adjacent(graph.contents, "A", "B"))->toBe(false)
  })

  test("get weight", () => {
    expect(StringAdjList.getWeight(graph.contents, "A", "B"))->toEqual(Some(1.0))
  })

  test("adjacent returns true for existing edge", () => {
    expect(StringAdjList.adjacent(graph.contents, "A", "B"))->toBe(true)
  })

  test("adjacent returns false for non-existing edge", () => {
    expect(StringAdjList.adjacent(graph.contents, "A", "D"))->toBe(false)
  })

  let neighbors_tests = list{
    ("A", ["B", "C"]),
    ("B", ["C", "D"]),
    ("C", ["D"]),
    ("D", ["E"]),
    ("E", ["A"]),
  }

  testAll("neighbors", neighbors_tests, ((node, expected)) => {
    expect(StringAdjList.neighbors(graph.contents, node))->toEqual(expected)
  })

  test("remove node", () => {
    StringAdjList.removeNode(graph.contents, "A")->ignore
    expect(StringAdjList.hasNode(graph.contents, "A"))->toBe(false)
  })
})

describe("AdjacencyList Tuple Implementation", () => {
  let graph = ref(TupleAdjList.make())

  beforeEach(() => {
    graph := TupleAdjList.make()
    TupleAdjList.addNode(graph.contents, ("A", 1))
    TupleAdjList.addNode(graph.contents, ("B", 2))
    TupleAdjList.addNode(graph.contents, ("C", 3))
    TupleAdjList.addNode(graph.contents, ("D", 4))
    TupleAdjList.addNode(graph.contents, ("E", 5))
    TupleAdjList.addDirectedEdge(graph.contents, ("A", 1), ("B", 2), ~weight=Some(1.0))
    TupleAdjList.addDirectedEdge(graph.contents, ("B", 2), ("C", 3), ~weight=Some(2.0))
    TupleAdjList.addDirectedEdge(graph.contents, ("C", 3), ("D", 4), ~weight=Some(3.0))
    TupleAdjList.addDirectedEdge(graph.contents, ("D", 4), ("E", 5), ~weight=Some(4.0))
    TupleAdjList.addDirectedEdge(graph.contents, ("E", 5), ("A", 1), ~weight=Some(5.0))
    TupleAdjList.addDirectedEdge(graph.contents, ("A", 1), ("C", 3), ~weight=Some(6.0))
    TupleAdjList.addDirectedEdge(graph.contents, ("B", 2), ("D", 4), ~weight=Some(7.0))
  })

  test("get all nodes", () => {
    let nodes = TupleAdjList.getAllNodes(graph.contents)
    expect(nodes)->toEqual([("A", 1), ("B", 2), ("C", 3), ("D", 4), ("E", 5)])
  })

  test("add node", () => {
    TupleAdjList.addNode(graph.contents, ("F", 6))
    expect(TupleAdjList.hasNode(graph.contents, ("F", 6)))->toEqual(true)
  })

  test("add edge", () => {
    expect(TupleAdjList.adjacent(graph.contents, ("A", 1), ("B", 2)))->toBe(true)
  })

  test("remove edge", () => {
    TupleAdjList.removeDirectedEdge(graph.contents, ("A", 1), ("B", 2))->ignore
    expect(TupleAdjList.adjacent(graph.contents, ("A", 1), ("B", 2)))->toBe(false)
  })

  test("get weight", () => {
    expect(TupleAdjList.getWeight(graph.contents, ("A", 1), ("B", 2)))->toEqual(Some(1.0))
  })

  test("adjacent returns true for existing edge in TupleAdjList", () => {
    expect(TupleAdjList.adjacent(graph.contents, ("A", 1), ("B", 2)))->toBe(true)
  })

  test("adjacent returns false for non-existing edge in TupleAdjList", () => {
    expect(TupleAdjList.adjacent(graph.contents, ("A", 1), ("D", 4)))->toBe(false)
  })

  let neighbors_tests = list{
    (("A", 1), [("B", 2), ("C", 3)]),
    (("B", 2), [("C", 3), ("D", 4)]),
    (("C", 3), [("D", 4)]),
    (("D", 4), [("E", 5)]),
    (("E", 5), [("A", 1)]),
  }

  testAll("neighbors", neighbors_tests, ((node, expected)) => {
    expect(TupleAdjList.neighbors(graph.contents, node))->toEqual(expected)
  })

  test("remove node", () => {
    TupleAdjList.removeNode(graph.contents, ("A", 1))->ignore
    expect(TupleAdjList.hasNode(graph.contents, ("A", 1)))->toBe(false)
  })
})
