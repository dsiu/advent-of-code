// ref: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#cite_note-Note2-20
//
module Dijkstra = (A: AdjacencyList.S) => {
  module PriorityQueue = PriorityQueue.MinPriorityQueue
  type nodeMap<'a> = Map.t<A.node, option<'a>>

  let dijkstra = (graph: A.t<'a>, startNode: A.node) => {
    let dist: Map.t<A.node, option<int>> = Map.make()
    let prev: Map.t<A.node, option<A.node>> = Map.make()

    // Initialize dist and priority queue
    graph
    ->A.getAllNodes
    ->Array.forEach(node => {
      dist->Map.set(node, None) // None means infinity
      prev->Map.set(node, None) // None means no previous node
    })
    dist->Map.set(startNode, Some(0))
    let pq = PriorityQueue.empty->PriorityQueue.push(0, startNode)

    let rec loop = (dist, prev, pq) => {
      switch PriorityQueue.pop(pq) {
      | exception Not_found => ()
      | (uDist, u, newQueue) =>
        graph
        ->A.neighbors(u)
        ->Array.forEach(v => {
          // need to check if the neighbor is still in the queue
          let edgeWeight: int = graph->A.getWeight(u, v)->Option.getExn
          //let dist_u: int = dist->Map.get(u)->Option.getExn->Option.getExn
          let alt: int = uDist + edgeWeight

          switch dist->Map.get(v) {
          | Some(Some(dist_v)) if alt < dist_v => {
              dist->Map.set(v, Some(alt))
              prev->Map.set(v, Some(u))
              loop(dist, prev, PriorityQueue.push(newQueue, alt, v))
            }

          | None => {
              // infinity
              dist->Map.set(v, Some(alt))
              prev->Map.set(v, Some(u))
              loop(dist, prev, PriorityQueue.push(newQueue, alt, v))
            }
          | _ => ()
          }
        })
      }
    }

    loop(dist, prev, pq)
    (dist, prev)
  }

  let shortestPaths = (graph: A.t<'a>, startNode: A.node) => {
    let (dist, prev) = dijkstra(graph, startNode)
    let rec reconstructPath = (prev, node, path) => {
      switch prev->Map.get(node) {
      | None => path
      | Some(None) => path
      | Some(Some(prevNode)) => reconstructPath(prev, prevNode, [prevNode, ...path])
      }
    }
    let paths: Map.t<A.node, array<A.node>> = Map.make()
    graph
    ->A.getAllNodes
    ->Array.forEach(node => {
      let path = reconstructPath(prev, node, [node])
      paths->Map.set(node, path)
    })
    paths
  }
}
