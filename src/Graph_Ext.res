open StdlibFp

let log = Console.log
let log2 = Console.log2

//module type S { type t module Sub : sig type t val to_outer : t/1 -> t/2 end end

module A = AdjacencyList.Node.String

// Belt's MutableQueue seems to be quite performant
module Queue = Belt.MutableQueue
module Stack = Belt.MutableStack

module Traversal = (A: AdjacencyList.S, NodeSet: StdlibFp.Set.S with type a = A.node) => {
  type traversalRecord<'a> = TraversalRecord({node: A.node, depth: int, from: option<A.node>})
  type callback = (A.node, int) => bool // return true to stop the traversal

  let bfs: (A.t<'a>, A.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
    let queue = Queue.make()
    queue->Queue.add(TraversalRecord({node: rootNode, depth: 0, from: None}))

    let rec loop = (graph, acc, visited, toVisit) => {
      toVisit->Queue.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth, from}) = toVisit->Queue.popExn
            visited->NodeSet.has(node)
              ? loop(graph, acc, visited, toVisit) // tail recursion
              : {
                  visited->NodeSet.add(node)
                  acc->Array.push(TraversalRecord({node, depth, from}))

                  !cb(node, depth)
                    ? {
                        graph
                        ->A.neighbors(node)
                        ->Array.valuesIter
                        ->Iterator.forEach(neighbor => {
                          switch neighbor {
                          | Some(neighborNode) =>
                            queue->Queue.add(
                              TraversalRecord({
                                node: neighborNode,
                                depth: depth + 1,
                                from: Some(node),
                              }),
                            )
                          | None => ()
                          }
                        })
                      }
                    : ()
                  // tail recursion
                  loop(graph, acc, visited, toVisit)
                }
          }
    }

    loop(graph, [], NodeSet.make(), queue)
  }

  let dfs: (A.t<'a>, A.node, callback) => array<traversalRecord<'a>> = (graph, rootNode, cb) => {
    let stack = Stack.make()
    stack->Stack.push(TraversalRecord({node: rootNode, depth: 0, from: None}))

    let rec loop = (graph, acc, visited, toVisit) => {
      toVisit->Stack.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth, from}) = toVisit->Stack.pop->Option.getExn
            visited->NodeSet.has(node)
              ? loop(graph, acc, visited, toVisit) // tail recursion
              : {
                  visited->NodeSet.add(node)
                  acc->Array.push(TraversalRecord({node, depth, from}))

                  !cb(node, depth)
                    ? {
                        graph
                        ->A.neighbors(node)
                        ->Array.valuesIter
                        ->Iterator.forEach(neighbor => {
                          switch neighbor {
                          | Some(neighborNode) =>
                            stack->Stack.push(
                              TraversalRecord({
                                node: neighborNode,
                                depth: depth + 1,
                                from: Some(node),
                              }),
                            )
                          | None => ()
                          }
                        })
                      }
                    : ()
                  // tail recursion
                  loop(graph, acc, visited, toVisit)
                }
          }
    }

    loop(graph, [], NodeSet.make(), stack)
  }

  let paths: array<traversalRecord<'a>> => array<array<A.node>> = records => {
    let rec buildPath = (record: traversalRecord<'a>, acc: array<A.node>): array<A.node> => {
      let TraversalRecord({node, depth: _depth, from}) = record
      switch from {
      | None => [node, ...acc]
      | Some(fromNode) =>
        let fromRecord =
          records->Array.find((TraversalRecord({node, _})) => node == fromNode)->Option.getExn
        buildPath(fromRecord, [node, ...acc])
      }
    }

    records->Array.map(record => buildPath(record, []))
  }

  let path: (array<traversalRecord<'a>>, A.node, A.node) => option<array<A.node>> = (
    records,
    a,
    b,
  ) => {
    let paths = paths(records)
    paths->Array.find(path => {
      path[0]->Option.filter(node => node == a)->Option.isSome &&
        path->Array.last->Option.filter(node => node == b)->Option.isSome
    })
  }
}
