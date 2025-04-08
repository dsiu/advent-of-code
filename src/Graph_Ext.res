open StdlibFp

let log = Console.log
let log2 = Console.log2

module type S = {
  type node
}

// Belt's MutableQueue seems to be quite performant
module Queue = Belt.MutableQueue
module Stack = Belt.MutableStack

module Traversal = (S: S, NodeSet: StdlibFp.Set.S with type a = S.node) => {
  type node = S.node

  type traversalRecord<'a> = TraversalRecord({node: node, depth: int, from: option<node>})
  type callback = (node, int) => bool // return true to stop the traversal

  type neighbors = node => array<node>

  let bfs: (node, neighbors, callback) => array<traversalRecord<'a>> = (
    rootNode,
    neighbors,
    cb,
  ) => {
    let queue = Queue.make()
    queue->Queue.add(TraversalRecord({node: rootNode, depth: 0, from: None}))

    let rec loop = (acc, visited, toVisit) => {
      toVisit->Queue.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth, from}) = toVisit->Queue.popExn
            visited->NodeSet.has(node)
              ? loop(acc, visited, toVisit) // tail recursion
              : {
                  visited->NodeSet.add(node)
                  acc->Array.push(TraversalRecord({node, depth, from}))

                  !cb(node, depth)
                    ? {
                        neighbors(node)
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
                  loop(acc, visited, toVisit)
                }
          }
    }

    loop([], NodeSet.make(), queue)
  }

  let dfs: (node, neighbors, callback) => array<traversalRecord<'a>> = (
    rootNode,
    neighbors,
    cb,
  ) => {
    let stack = Stack.make()
    stack->Stack.push(TraversalRecord({node: rootNode, depth: 0, from: None}))

    let rec loop = (acc, visited, toVisit) => {
      toVisit->Stack.isEmpty
        ? acc
        : {
            let TraversalRecord({node, depth, from}) = toVisit->Stack.pop->Option.getExn
            visited->NodeSet.has(node)
              ? loop(acc, visited, toVisit) // tail recursion
              : {
                  visited->NodeSet.add(node)
                  acc->Array.push(TraversalRecord({node, depth, from}))

                  !cb(node, depth)
                    ? {
                        neighbors(node)
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
                  loop(acc, visited, toVisit)
                }
          }
    }

    loop([], NodeSet.make(), stack)
  }

  let paths: array<traversalRecord<'a>> => array<array<node>> = records => {
    let rec buildPath = (record: traversalRecord<'a>, acc: array<node>): array<node> => {
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

  let path: (array<traversalRecord<'a>>, node, node) => option<array<node>> = (records, a, b) => {
    let paths = paths(records)
    paths->Array.find(path => {
      path[0]->Option.filter(node => node == a)->Option.isSome &&
        path->Array.last->Option.filter(node => node == b)->Option.isSome
    })
  }
}
