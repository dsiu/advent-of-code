open Belt
open Utils
let log = Js.Console.log

module Drawn = {
  type t = array<int>

  let make = Js.String2.split(_, ",")
}

module Board = {
  type t = Array2D.t<int>

  let make = (lines): t => {
    lines->Array.map(x =>
      x
      ->Js.String2.split(" ")
      ->Array.keep(s => s->Js.String2.length(_) > 0)
      ->Array.map(s => s->Js.String2.trim->Int.fromString->Option.getExn)
    )
  }

  let match = (candidates, match_keys) => {
    let can_set = candidates->Set.Int.fromArray
    let keys_set = match_keys->Set.Int.fromArray
    Set.Int.subset(keys_set, can_set) ? Some(can_set->Set.Int.toArray) : None
  }

  let solve = (t, match_keys) => {
    let rec helper = (t, iy) => {
      iy < t->Array2D.lengthY
        ? t
          ->Array2D.getYEquals(iy)
          ->Option.flatMap(row => {
            switch row->match(_, match_keys) {
            | Some(matched) => Some(matched)
            | None => helper(t, iy + 1)
            }
          })
        : None
    }
    helper(t, 0)
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
