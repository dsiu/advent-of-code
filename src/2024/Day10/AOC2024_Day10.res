open StdlibFp
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t // row, col

type trail = array<position>

type grid = Array2D.t<int>

type tMap = {
  grid: grid,
  starts: array<position>,
  goals: array<position>,
}

type agendum = {
  current: position,
  trail: trail,
}

type agenda = array<agendum>

let parse: string => Array2D.t<int> = data => {
  data
  ->splitNewline
  ->Array.map(Fn.compose(String.trim, Utils.splitChars, _))
  ->Array.reduceWithIndex([], (acc, row, _r) => {
    [...acc, row->Array.mapWithIndex((col, _c) => Int.fromString(col)->Option.getOr(-1))]
  })
}

let solvePart1 = data => {
  let grid = data->parse

  // todo: refactor to Array2D.filterWithIndex ??
  let starts =
    grid->Array2D.reduceWithIndex([], (acc, v, (c, r)) => {v == 0 ? [...acc, (r, c)] : acc}) // remember position is (row, col)
  let goals =
    grid->Array2D.reduceWithIndex([], (acc, v, (c, r)) => {v == 9 ? [...acc, (r, c)] : acc}) // remember position is (row, col)
  let tMap = {grid, starts, goals}
  tMap->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
