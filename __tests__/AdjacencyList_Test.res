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
    StringAdjList.addDirectedEdge(graph.contents, "A", "B")
    StringAdjList.addDirectedEdge(graph.contents, "B", "C")
    StringAdjList.addDirectedEdge(graph.contents, "C", "D")
    StringAdjList.addDirectedEdge(graph.contents, "D", "E")
    StringAdjList.addDirectedEdge(graph.contents, "E", "A")
    StringAdjList.addDirectedEdge(graph.contents, "A", "C")
    StringAdjList.addDirectedEdge(graph.contents, "B", "D")
  })

  test("add vertex", () => {
    StringAdjList.addNode(graph.contents, "F")
    expect(StringAdjList.getNode(graph.contents, "F"))->toEqual([])
  })

  test("add edge", () => {
    expect(StringAdjList.adjacent(graph.contents, "A", "B"))->toBe(true)
  })

  test("remove edge", () => {
    StringAdjList.removeDirectedEdge(graph.contents, "A", "B")->ignore
    expect(StringAdjList.adjacent(graph.contents, "A", "B"))->toBe(false)
  })

  let neighbors_tests = list{
    ("A", ["B", "C"]),
    ("B", ["C", "D"]),
    ("C", ["D"]),
    ("D", ["E"]),
    ("E", ["A"]),
  }

  testAll("neighbors", neighbors_tests, ((vertex, expected)) => {
    expect(StringAdjList.neighbors(graph.contents, vertex))->toEqual(expected)
  })

  test("remove vertex", () => {
    StringAdjList.removeNode(graph.contents, "A")->ignore
    let fn = () => StringAdjList.getNode(graph.contents, "A")
    expect(fn)->toThrow
  })

  //  test("toString", () => {
  //    expect(StringAdjList.toString(graph.contents))->toBe(
  //      "A: [ B,C ]\nB: [ C,D ]\nC: [ D ]\nD: [ E ]\nE: [ A ]\n",
  //    )
  //  })
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
    TupleAdjList.addDirectedEdge(graph.contents, ("A", 1), ("B", 2))
    TupleAdjList.addDirectedEdge(graph.contents, ("B", 2), ("C", 3))
    TupleAdjList.addDirectedEdge(graph.contents, ("C", 3), ("D", 4))
    TupleAdjList.addDirectedEdge(graph.contents, ("D", 4), ("E", 5))
    TupleAdjList.addDirectedEdge(graph.contents, ("E", 5), ("A", 1))
    TupleAdjList.addDirectedEdge(graph.contents, ("A", 1), ("C", 3))
    TupleAdjList.addDirectedEdge(graph.contents, ("B", 2), ("D", 4))
  })

  test("add vertex", () => {
    TupleAdjList.addNode(graph.contents, ("F", 6))
    expect(TupleAdjList.getNode(graph.contents, ("F", 6)))->toEqual([])
  })

  test("add edge", () => {
    expect(TupleAdjList.adjacent(graph.contents, ("A", 1), ("B", 2)))->toBe(true)
  })

  test("remove edge", () => {
    TupleAdjList.removeDirectedEdge(graph.contents, ("A", 1), ("B", 2))->ignore
    expect(TupleAdjList.adjacent(graph.contents, ("A", 1), ("B", 2)))->toBe(false)
  })

  let neighbors_tests = list{
    (("A", 1), [("B", 2), ("C", 3)]),
    (("B", 2), [("C", 3), ("D", 4)]),
    (("C", 3), [("D", 4)]),
    (("D", 4), [("E", 5)]),
    (("E", 5), [("A", 1)]),
  }

  testAll("neighbors", neighbors_tests, ((vertex, expected)) => {
    expect(TupleAdjList.neighbors(graph.contents, vertex))->toEqual(expected)
  })

  test("remove vertex", () => {
    TupleAdjList.removeNode(graph.contents, ("A", 1))->ignore
    let fn = () => TupleAdjList.getNode(graph.contents, ("A", 1))
    expect(fn)->toThrow
  })

  //  test("toString", () => {
  //    expect(TupleAdjList.toString(graph.contents))->toBe(
  //      "A,1: [ B,2,C,3 ]\nB,2: [ C,3,D,4 ]\nC,3: [ D,4 ]\nD,4: [ E,5 ]\nE,5: [ A,1 ]\n",
  //    )
  //  })
})
