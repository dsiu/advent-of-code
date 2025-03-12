open Jest
open Expect

module StringAdjList = AdjacencyList.String
module TupleAdjList = AdjacencyList.Tuple

describe("AdjacencyList String Implementation", () => {
  let graph = ref(StringAdjList.make())

  beforeEach(() => {
    graph := StringAdjList.make()
    StringAdjList.addVertex(graph.contents, "A")
    StringAdjList.addVertex(graph.contents, "B")
    StringAdjList.addVertex(graph.contents, "C")
    StringAdjList.addVertex(graph.contents, "D")
    StringAdjList.addVertex(graph.contents, "E")
    StringAdjList.addEdge(graph.contents, "A", "B")
    StringAdjList.addEdge(graph.contents, "B", "C")
    StringAdjList.addEdge(graph.contents, "C", "D")
    StringAdjList.addEdge(graph.contents, "D", "E")
    StringAdjList.addEdge(graph.contents, "E", "A")
    StringAdjList.addEdge(graph.contents, "A", "C")
    StringAdjList.addEdge(graph.contents, "B", "D")
  })

  test("add vertex", () => {
    StringAdjList.addVertex(graph.contents, "F")
    expect(StringAdjList.getVertex(graph.contents, "F"))->toEqual([])
  })

  test("add edge", () => {
    expect(StringAdjList.adjacent(graph.contents, "A", "B"))->toBe(true)
  })

  test("remove edge", () => {
    StringAdjList.removeEdge(graph.contents, "A", "B")
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
    StringAdjList.removeVertex(graph.contents, "A")
    let fn = () => StringAdjList.getVertex(graph.contents, "A")
    expect(fn)->toThrow
  })

  test("toString", () => {
    expect(StringAdjList.toString(graph.contents))->toBe(
      "A: [ B,C ]\nB: [ C,D ]\nC: [ D ]\nD: [ E ]\nE: [ A ]\n",
    )
  })
})

describe("AdjacencyList Tuple Implementation", () => {
  let graph = ref(TupleAdjList.make())

  beforeEach(() => {
    graph := TupleAdjList.make()
    TupleAdjList.addVertex(graph.contents, "A")
    TupleAdjList.addVertex(graph.contents, "B")
    TupleAdjList.addVertex(graph.contents, "C")
    TupleAdjList.addVertex(graph.contents, "D")
    TupleAdjList.addVertex(graph.contents, "E")
    TupleAdjList.addEdge(graph.contents, "A", ("B", 1))
    TupleAdjList.addEdge(graph.contents, "B", ("C", 2))
    TupleAdjList.addEdge(graph.contents, "C", ("D", 3))
    TupleAdjList.addEdge(graph.contents, "D", ("E", 4))
    TupleAdjList.addEdge(graph.contents, "E", ("A", 5))
    TupleAdjList.addEdge(graph.contents, "A", ("C", 6))
    TupleAdjList.addEdge(graph.contents, "B", ("D", 7))
  })

  test("add vertex", () => {
    TupleAdjList.addVertex(graph.contents, "F")
    expect(TupleAdjList.getVertex(graph.contents, "F"))->toEqual([])
  })

  test("add edge", () => {
    expect(TupleAdjList.adjacent(graph.contents, "A", ("B", 1)))->toBe(true)
  })

  test("remove edge", () => {
    TupleAdjList.removeEdge(graph.contents, "A", ("B", 1))
    expect(TupleAdjList.adjacent(graph.contents, "A", ("B", 1)))->toBe(false)
  })

  let neighbors_tests = list{
    ("A", [("B", 1), ("C", 6)]),
    ("B", [("C", 2), ("D", 7)]),
    ("C", [("D", 3)]),
    ("D", [("E", 4)]),
    ("E", [("A", 5)]),
  }

  testAll("neighbors", neighbors_tests, ((vertex, expected)) => {
    expect(TupleAdjList.neighbors(graph.contents, vertex))->toEqual(expected)
  })

  test("remove vertex", () => {
    TupleAdjList.removeVertex(graph.contents, "A")
    let fn = () => TupleAdjList.getVertex(graph.contents, "A")
    expect(fn)->toThrow
  })

  test("toString", () => {
    expect(TupleAdjList.toString(graph.contents))->toBe(
      "A: [ B,1,C,6 ]\nB: [ C,2,D,7 ]\nC: [ D,3 ]\nD: [ E,4 ]\nE: [ A,5 ]\n",
    )
  })
})
