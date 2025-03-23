// ref: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#cite_note-Note2-20
//

module type S = {
  type t<'a>
  type node
  let dijkstra: (t<'a>, node) => int
}

module Dijkstra = (A: AdjacencyList.S, NodeMap: StdlibFp.Map.S with type key = A.node) => {
  type t<'a>
  type node = A.node
  module PriorityQueue = PriorityQueue.MinPriorityQueue

  //  type nodeMap<'a> = NodeMap.t<A.node, option<'a>>

  let dijkstra = (graph: A.t<'a>, startNode: A.node) => {
    //    let dist: NodeMap.t<A.node, option<int>> = NodeMap.make()
    //    let prev: NodeMap.t<A.node, option<A.node>> = NodeMap.make()
    let dist = NodeMap.make()
    let prev = NodeMap.make()

    // Initialize dist and priority queue
    graph
    ->A.getAllNodes
    ->Array.forEach(node => {
      dist->NodeMap.set(node, None) // None means infinity
      prev->NodeMap.set(node, None) // None means no previous node
    })
    dist->NodeMap.set(startNode, Some(0))
    let pq = PriorityQueue.empty->PriorityQueue.push(0, startNode)

    let rec loop = (dist, prev, pq) => {
      switch PriorityQueue.pop(pq) {
      | exception Not_found => ()
      | (uDist, u, newQueue) => {
          graph->A.neighbors(u)->Console.log4("dijkstra:", u, "has neighbors: ", _)
          graph
          ->A.neighbors(u)
          ->Array.forEach(v => {
            // need to check if the neighbor is still in the queue
            let edgeWeight: int = graph->A.getWeight(u, v)->Option.getExn
            //let dist_u: int = dist->Map.get(u)->Option.getExn->Option.getExn
            let alt: int = uDist + edgeWeight

            switch dist->NodeMap.get(v) {
            | Some(Some(dist_v)) if alt < dist_v => {
                dist->NodeMap.set(v, Some(alt))
                prev->NodeMap.set(v, Some(u))
                loop(dist, prev, PriorityQueue.push(newQueue, alt, v))
              }

            | None => {
                // infinity
                dist->NodeMap.set(v, Some(alt))
                prev->NodeMap.set(v, Some(u))
                loop(dist, prev, PriorityQueue.push(newQueue, alt, v))
              }
            | _ => ()
            }
          })
        }
      }
    }

    loop(dist, prev, pq)
    (dist, prev)
  }

  let shortestPaths = (graph: A.t<'a>, startNode: A.node) => {
    let (dist, prev) = dijkstra(graph, startNode)
    let rec reconstructPath = (prev, node, path) => {
      switch prev->NodeMap.get(node) {
      | None => path
      | Some(None) => path
      | Some(Some(prevNode)) => reconstructPath(prev, prevNode, [prevNode, ...path])
      }
    }
    let paths: NodeMap.t<A.node, array<A.node>> = NodeMap.make()
    graph
    ->A.getAllNodes
    ->Array.forEach(node => {
      let path = reconstructPath(prev, node, [node])
      paths->NodeMap.set(node, path)
    })
    paths
  }

  let shortestPath = (graph: A.t<'a>, startNode: A.node, endNode: A.node) => {
    shortestPaths(graph, startNode)->NodeMap.get(endNode)
  }
}
