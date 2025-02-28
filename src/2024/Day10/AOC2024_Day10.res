open StdlibFp
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t

type trail = array<position>

type grid = array<(position, int)>

type tMap = {
  grid: grid,
  starts: array<position>,
  goals: array<position>,
}

type agendum = {
  current: position,
  tail: trail,
}

type agenda = array<agendum>

let parse = data => {
  data
  ->splitNewline
  ->Array.map(Fn.compose(String.trim, Utils.splitChars, _))
  ->Array.reduceWithIndex([], (acc, row, r) => {
    [
      ...acc,
      ...row->Array.mapWithIndex((col, c) => ((r, c), Int.fromString(col)->Option.getOr(-1))),
    ]
  })
}

let solvePart1 = data => {
  let grid = data->parse
  let starts = grid->Array.filter(((_, v)) => v == 0)->Array.map(fst)
  let goals = grid->Array.filter(((_, v)) => v == 9)->Array.map(fst)
  let tMap = {grid, starts, goals}
  tMap->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
