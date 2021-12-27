open Belt
open Utils
let log = Js.Console.log

module Drawn = {
  type t = array<int>

  let make = Js.String2.split(_, ",")
}

module Board = {
  type t = Array2D.t<int>

  let make = lines => {
    lines->Array.map(x =>
      x
      ->Js.String2.split(" ")
      ->Array.keep(s => s->Js.String2.length(_) > 0)
      ->Array.map(s => s->Js.String2.trim->Int.fromString)
    )
  }
}

module Boards = {
  type t = array<Board.t>

  let make = lines => {
    lines->Array.map(b => {
      b->splitNewline->Array.map(Js.String2.trim)->Board.make
    })
  }

  let dump = t => t->Array.forEach(Js.log)
}

let part1 = (drawn, boards) => {
  boards->Boards.dump
  (drawn, boards)
}

let parse = data => {
  let lines =
    data
    ->splitDoubleNewline
    ->Array.map(x => {
      x->Js.String2.trim
    })
  let drawn = lines[0]->Option.getExn->Drawn.make
  let boards = lines->Array.sliceToEnd(1)->Boards.make
  (drawn, boards)
}

let solvePart1 = data => {
  let (drawn, boards) = data->parse
  part1(drawn, boards)
}

let solvePart2 = data => {
  data->ignore
  2
}
