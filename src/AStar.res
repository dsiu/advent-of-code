// ref:https://en.wikipedia.org/wiki/A*_search_algorithm

module AStar = (
  A: AdjacencyList.S,
  NodeMap: StdlibFp.Map.S with type key = A.node,
  NodeSet: StdlibFp.Set.S with type a = A.node,
) => {
  module PriorityQueue = PriorityQueue.MinPriorityQueue

  type node = A.node
  type gScore = NodeMap.t<node, float>
  type fScore = NodeMap.t<node, float>
  type cameFrom = NodeMap.t<node, node>
  type openSet_S = NodeSet.t<node>
  type openSet_Q = PriorityQueue.queue<float, node>
  type openSet = (openSet_Q, openSet_S)

  let s: openSet_S = NodeSet.make()

  let infinity = Float.Constants.positiveInfinity

  // helper function to add a node to the open set
  let addToOpenSet = (os: openSet, node: A.node, fValue: float) => {
    let (queue, set) = os
    (
      queue->PriorityQueue.push(fValue, node),
      {
        NodeSet.add(set, node)
        set
      },
    )
  }

  // helper function to check if a node is in open set
  let isInOpenSet = ((_, set), node) => set->NodeSet.has(node)

  // helper function to remove a node from the open set
  let removeFromOpenSet = ((queue, set)) => {
    let queueOpt = queue->PriorityQueue.pop
    switch queueOpt {
    | None => (None, (queue, set))
    | Some(_, node, queue') => (
        Some(node),
        (
          queue',
          {
            set->NodeSet.delete(node)->ignore
            set
          },
        ),
      )
    }
  }

  let rec reconstructPath = (cameFrom, current) => {
    switch cameFrom->NodeMap.get(current) {
    | None => [current]
    | Some(previous) => [...reconstructPath(cameFrom, previous), current]
    }
  }

  type heuristic = (A.node, A.node) => float
  type cost = (A.node, A.node) => float
  type neighbors = A.node => array<A.node>

  let aStar = (
    start: A.node,
    goal: A.node,
    neighbors: neighbors,
    cost: cost,
    heuristic: heuristic,
  ) => {
    let initialOpenSet = addToOpenSet(
      (PriorityQueue.empty, NodeSet.make()),
      start,
      heuristic(start, goal),
    )

    let initGScore = NodeMap.make()
    initGScore->NodeMap.set(start, 0.)
    let initFScore = NodeMap.make()
    initFScore->NodeMap.set(start, heuristic(start, goal))
    let initCameFrom = NodeMap.make()
    let initClosedSet = NodeSet.make()

    let processNeighbor = (current, (openSet, closedSet, gScore, fScore, cameFrom), neighbor) => {
      closedSet->NodeSet.has(neighbor)
        ? (openSet, closedSet, gScore, fScore, cameFrom)
        : {
            let currentG = gScore->NodeMap.get(current)->Option.getWithDefault(infinity)
            let tentativeG = currentG + cost(current, neighbor)
            let neighborG = gScore->NodeMap.get(neighbor)->Option.getWithDefault(infinity)

            tentativeG < neighborG
              ? {
                  gScore->NodeMap.set(neighbor, tentativeG)
                  let newGScore = gScore

                  let fValue = tentativeG + heuristic(neighbor, goal)
                  fScore->NodeMap.set(neighbor, fValue)
                  let newFScore = fScore

                  cameFrom->NodeMap.set(neighbor, current)
                  let newCameFrom = cameFrom

                  let newOpenSet =
                    openSet->isInOpenSet(neighbor)
                      ? openSet
                      : openSet->addToOpenSet(neighbor, fValue)
                  (newOpenSet, closedSet, newGScore, newFScore, newCameFrom)
                }
              : (openSet, closedSet, gScore, fScore, cameFrom)
          }
    }

    // todo: neighbors should be a callback instead of a adjlist
    let rec search = ((openSet, closedSet, gScore, fScore, cameFrom)) => {
      let (currentOpt, newOpenSet) = openSet->removeFromOpenSet
      switch currentOpt {
      | None => None
      | Some(current) =>
        current == goal
          ? Some(reconstructPath(cameFrom, goal))
          : {
              closedSet->NodeSet.add(current)
              let newClosedSet = closedSet

              // todo: neighbors should be a callback instead of a adjlist
              let neighborsList = neighbors(current)

              let newState = neighborsList->Array.reduce(
                (newOpenSet, newClosedSet, gScore, fScore, cameFrom),
                (acc, neighbor) => {
                  processNeighbor(current, acc, neighbor)
                },
              )
              search(newState)
            }
      }
    }

    search((initialOpenSet, initClosedSet, initGScore, initFScore, initCameFrom))
  }
}
