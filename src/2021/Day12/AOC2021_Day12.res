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

let parse = data =>
  data->splitNewline->Array.map(Js.String2.trim)->Array.map(Js.String2.split(_, "-"))

let solvePart1 = data => {
  data->parse->Maze.make->Maze.toString->Js.log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
