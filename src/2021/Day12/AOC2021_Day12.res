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

//let array_count = (xs, predicate) => xs->Array.keep(predicate)->Array.size

//let count_node_in_array = (a, node: string) => array_count(a, FP_Utils.eq(node))
//let is_visited = (visited, node) => count_node_in_array(visited, node) > 0

let is_visited = (visited, node) => {
  visited->HashMap.String.get(node)->Option.mapWithDefault(false, v => v > 0)
}

// should refactor this. 2021 Day 14 uses this too

let inc_visited = (visited, node) => {
  visited->hashMapStringUpdate(node, increaseBy1)
}

let get_visited_count = (visited, node) => {
  visited->HashMap.String.get(node)->Option.getWithDefault(0)
}

let is_upper_case = c => c->Js.String2.toUpperCase === c
let is_small_case = c => c->Js.String2.toLowerCase === c
let is_big_cave = is_upper_case
let is_small_cave = is_small_case

let has_any_small_cave_been_visited_twice = visited => {
  let smalls = visited->HashMap.String.reduce([], (acc, key, value) => {
    is_small_cave(key) && value > 1 ? Array.concat(acc, [(key, value)]) : acc
  })
  smalls->Array.length > 0
}

let get_edges = (t, node) => Adjacency_List.neighbors(t, node)

let can_visit_part1 = (visited, node) => is_big_cave(node) || get_visited_count(visited, node) <= 0

let can_visit_part2 = (visited, node) => {
  let c = get_visited_count(visited, node)
  is_big_cave(node) ||
  {
    switch node {
    | "start"
    | "end" =>
      c <= 0
    | _ => c <= 0 || !has_any_small_cave_been_visited_twice(visited)
    }
  }
}

let get_visited_nodes = HashMap.String.keysToArray

let dfs = (visit_func, t: Maze.t, start_node, end_node) => {
  let rec explore = (node, visited, acc, end_node) => {
    let visited' = inc_visited(visited, node)

    switch node == end_node {
    | false => {
        let edges = t->get_edges(node)
        edges->HashSet.String.reduce([], (a, e) => {
          visit_func(visited', e)
            ? {
                Array.concat(
                  a,
                  explore(e, visited'->HashMap.String.copy, Array.concat(acc, [e]), end_node),
                )
              }
            : {
                a
              }
        })
      }
    | true => [acc]
    }
  }

  let visited = Belt.HashMap.String.make(~hintSize=40)
  let acc = [start_node]
  explore(start_node, visited, acc, end_node)
}

let dfs_part1 = dfs(can_visit_part1)
let dfs_part2 = dfs(can_visit_part2)

let array_to_string = xs => {
  xs->Array.reduce("", (acc, x) => {
    acc ++ " " ++ x
  })
}

let sort_result = result => {
  result->Array.map(array_to_string)->SortArray.String.stableSort
}

let parse = data =>
  data->splitNewline->Array.map(Js.String2.trim)->Array.map(Js.String2.split(_, "-"))

let solvePart1 = data => {
  let maze = data->parse->Maze.make
  //  maze->Maze.toString->Js.log
  let result = maze->dfs_part1("start", "end")
  //  result->sort_result->Js.log2("part 1: ", _)
  //  result->Array.size->Js.log
  result->Array.size
}

let solvePart2 = data => {
  let maze = data->parse->Maze.make
  //  maze->Maze.toString->Js.log
  let result = maze->dfs_part2("start", "end")
  //  result->sort_result->Js.log2("part 2: ", _)
  //  result->Array.size->Js.log
  result->Array.size
}
