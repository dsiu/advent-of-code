//open Stdlib
//
//type nodePaths = NodePath({node: string, end: string, paths: Map.t<string, int>})
//
//let updatePath = (NodePath({node, end, paths}), entry) => {
//  //let currentDistance
//}
//
//let rec recurseFindShortestPath: nodePaths => nodePaths = nodePaths => {
//  nodePaths->isFinished()
//    ? nodePaths
//    : {
//        let nextNode = nodePaths->getNextNode(weightedPaths)
//        recurseFindShortestPath(nextNode)
//      }
//}
//
//let findShortestPath: (string, string) => int = {
//  let paths = recurseFindShortestPath(NodePaths(start, end)).paths
//  paths->Map.get(end)
//}
