// ref:https://en.wikipedia.org/wiki/A*_search_algorithm

/**
 * Module type for nodes in the A* algorithm
 */
module type S = {
  type node
}

/**
 * A* Search Algorithm
 *
 * This module implements the A* pathfinding algorithm optimized for performance
 * while maintaining clarity and handling edge cases gracefully.
 */
module AStar = (
  S: S,
  NodeMap: StdlibFp.Map.S with type key = S.node,
  NodeSet: StdlibFp.Set.S with type a = S.node,
) => {
  module PriorityQueue = PriorityQueue.MinPriorityQueue

  /**
   * Type definitions for the A* algorithm
   */
  type node = S.node
  type gScore = NodeMap.t<node, float>
  type fScore = NodeMap.t<node, float>
  type cameFrom = NodeMap.t<node, node>
  type openSet_S = NodeSet.t<node>
  type openSet_Q = PriorityQueue.queue<float, node>
  type openSet = (openSet_Q, openSet_S)

  /**
   * Constant representing infinity for distance calculations
   */
  let infinity = Float.Constants.positiveInfinity

  /**
   * Function type definitions
   */
  type heuristic = (node, node) => float
  type cost = (node, node) => float
  type neighbors = node => array<node>

  /**
   * Adds a node to the open set with its f-score
   *
   * @param os - The current open set
   * @param node - The node to add
   * @param fValue - The f-score of the node
   * @return The updated open set containing the node
   */
  let addToOpenSet = (os: openSet, node: node, fValue: float) => {
    let (queue, set) = os
    (
      queue->PriorityQueue.push(fValue, node),
      {
        NodeSet.add(set, node)
        set
      },
    )
  }

  /**
   * Checks if a node is in the open set
   *
   * @param os - The open set
   * @param node - The node to check
   * @return True if the node is in the open set, false otherwise
   */
  let isInOpenSet = ((_, set), node) => set->NodeSet.has(node)

  /**
   * Removes the node with the lowest f-score from the open set
   *
   * @param os - The open set
   * @return A tuple containing the removed node (if any) and the updated open set
   */
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

  /**
   * Reconstructs the path from start to goal using the cameFrom map
   *
   * @param cameFrom - Map tracking the optimal path to each node
   * @param current - The current node (initially the goal)
   * @return Array representing the path from start to goal
   */
  let rec reconstructPath = (cameFrom, current) => {
    switch cameFrom->NodeMap.get(current) {
    | None => [current]
    | Some(previous) => [...reconstructPath(cameFrom, previous), current]
    }
  }

  /**
   * Process a neighbor node during A* search
   *
   * @param current - Current node being processed
   * @param state - Current algorithm state
   * @param neighbor - Neighbor node to process
   * @param goal - The goal node
   * @param cost - Cost function
   * @param heuristic - Heuristic function
   * @return Updated algorithm state
   */
  let processNeighbor = (
    current,
    (openSet, closedSet, gScore, fScore, cameFrom),
    neighbor,
    goal,
    cost,
    heuristic,
  ) => {
    // Skip if neighbor is in closed set (already processed)
    if closedSet->NodeSet.has(neighbor) {
      (openSet, closedSet, gScore, fScore, cameFrom)
    } else {
      // Calculate tentative g-score
      let currentG = gScore->NodeMap.get(current)->Option.getWithDefault(infinity)
      let tentativeG = currentG +. cost(current, neighbor)
      let neighborG = gScore->NodeMap.get(neighbor)->Option.getWithDefault(infinity)

      // Update if better path found
      if tentativeG < neighborG {
        // Update g-score
        gScore->NodeMap.set(neighbor, tentativeG)
        let newGScore = gScore

        // Update f-score
        let fValue = tentativeG +. heuristic(neighbor, goal)
        fScore->NodeMap.set(neighbor, fValue)
        let newFScore = fScore

        // Update came-from
        cameFrom->NodeMap.set(neighbor, current)
        let newCameFrom = cameFrom

        // Update open set if needed
        let newOpenSet =
          openSet->isInOpenSet(neighbor) ? openSet : openSet->addToOpenSet(neighbor, fValue)

        (newOpenSet, closedSet, newGScore, newFScore, newCameFrom)
      } else {
        // Keep current state
        (openSet, closedSet, gScore, fScore, cameFrom)
      }
    }
  }

  /**
   * Recursive search function for A* algorithm
   *
   * @param state - Current algorithm state (openSet, closedSet, gScore, fScore, cameFrom)
   * @param goal - The goal node
   * @param neighbors - Function that returns adjacent nodes
   * @param cost - Function that returns cost between adjacent nodes
   * @param heuristic - Function that estimates cost to goal
   * @return Option containing path from start to goal, or None if no path exists
   */
  let rec search = (
    (openSet, closedSet, gScore, fScore, cameFrom),
    goal,
    neighbors,
    cost,
    heuristic,
  ) => {
    let (currentOpt, newOpenSet) = openSet->removeFromOpenSet

    switch currentOpt {
    | None => None // No path found
    | Some(current) =>
      // Check if goal reached
      if current == goal {
        Some(reconstructPath(cameFrom, goal))
      } else {
        // Add current node to closed set
        closedSet->NodeSet.add(current)
        let newClosedSet = closedSet

        // Get neighbors
        let neighborsList = neighbors(current)

        // Process each neighbor
        let newState = neighborsList->Array.reduce(
          (newOpenSet, newClosedSet, gScore, fScore, cameFrom),
          (acc, neighbor) => {
            processNeighbor(current, acc, neighbor, goal, cost, heuristic)
          },
        )

        // Continue search
        search(newState, goal, neighbors, cost, heuristic)
      }
    }
  }

  /**
   * A* pathfinding algorithm
   *
   * @param start - Starting node
   * @param goal - Destination node
   * @param neighbors - Function that returns adjacent nodes
   * @param cost - Function that returns cost between adjacent nodes
   * @param heuristic - Function that estimates cost to goal
   * @return Option containing path from start to goal, or None if no path exists
   */
  let aStar = (start: node, goal: node, neighbors: neighbors, cost: cost, heuristic: heuristic) => {
    // Special case: start and goal are the same node
    if start == goal {
      Some([start])
    } else {
      // Initialize open set with start node
      let initialOpenSet = addToOpenSet(
        (PriorityQueue.empty, NodeSet.make()),
        start,
        heuristic(start, goal),
      )

      // Initialize score maps
      let initGScore = NodeMap.make()
      initGScore->NodeMap.set(start, 0.)
      let initFScore = NodeMap.make()
      initFScore->NodeMap.set(start, heuristic(start, goal))

      // Initialize tracking maps
      let initCameFrom = NodeMap.make()
      let initClosedSet = NodeSet.make()

      // Start the search
      search(
        (initialOpenSet, initClosedSet, initGScore, initFScore, initCameFrom),
        goal,
        neighbors,
        cost,
        heuristic,
      )
    }
  }
}
