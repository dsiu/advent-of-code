open Belt
open Utils
let log = Js.Console.log

module Maze = {
  type t = Adjacency_List.t

  exception ParseError

  let make = edges => {
    let maze = Adjacency_List.make(~hintSize=40)
    edges->Array.forEach(edge => {
      switch edge {
      | [a, b] => {
          maze->Adjacency_List.addEdge(a, b)
          maze->Adjacency_List.addEdge(b, a)
        }
      | _ => raise(ParseError)
      }
    })
    maze
  }

  let toString = Adjacency_List.toString
}

let array_count = (xs, predicate) => xs->Array.keep(predicate)->Array.size

//module Tree = {
//  type rec t =
//    | Leaf
//    | Node(string, Children(array<t>))
//}

exception CycleFound

let count_node_in_array = (a, node: string) => array_count(a, FP_Utils.eq(node))
let is_visited = (visited, node) => count_node_in_array(visited, node) > 0
let get_edges = (t, node) => Adjacency_List.neighbors(t, node)

let is_upper_case = c => c->Js.String2.toUpperCase === c

let can_visited_part1 = (visited, node) =>
  is_upper_case(node) || count_node_in_array(visited, node) <= 0

let can_visited_part2 = (visited, node) => {
  let c = count_node_in_array(visited, node)

  switch node {
  | "start"
  | "end" =>
    c < 1
  | _ => is_upper_case(node) || c < 2 // need to find out just ONE small case can be visited twice
  }
}

let dfs = (visit_func, t: Maze.t, start_node, end_node) => {
  let rec explore = (node, visited, path, acc, end_node) => {
    let visited' = Array.concat(visited, [node])

    switch node == end_node {
    | false => {
        //        Js.log(`explore: ${node}`)
        //        Js.log2("visited: ", visited)

        let edges = t->get_edges(node)

        //        Js.log2("edges:", edges->HashSet.String.toArray)

        let ret = edges->HashSet.String.reduce([], (a, e) => {
          visit_func(visited', e)
            ? Array.concat(a, explore(e, visited', path, Array.concat(visited', [e]), end_node))
            : a
        })

        //        ret->Js.log2("ret:", _)
        ret
      }
    | true => [acc]
    }
  }

  let visited = [] //Belt.HashMap.String.make()
  let path = []
  let acc = []
  explore(start_node, visited, path, acc, end_node)
}

let dfs_part1 = dfs(can_visited_part1)
let dfs_part2 = dfs(can_visited_part2)

let parse = data =>
  data->splitNewline->Array.map(Js.String2.trim)->Array.map(Js.String2.split(_, "-"))

let solvePart1 = data => {
  let maze = data->parse->Maze.make
  maze->Maze.toString->Js.log
  let result = maze->dfs_part1("start", "end")
  //  result->Js.log2("part 1: ", _)
  result->Array.size->Js.log
  result->Array.size
}

let solvePart2 = data => {
  let maze = data->parse->Maze.make
  maze->Maze.toString->Js.log
  let result = maze->dfs_part2("start", "end")
  result->Js.log2("part 2: ", _)
  result->Array.size->Js.log
  result->Array.size
}
