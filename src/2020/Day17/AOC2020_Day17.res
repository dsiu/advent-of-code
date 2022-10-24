// ref: https://work.njae.me.uk/2020/12/26/advent-of-code-2020-day-17/

open Belt
open Utils
open FP_Utils

let log = Js.Console.log

module TC = Tablecloth

module Coord = {
  open TC
  type t = Tuple3.t<int, int, int>
  let compare = Tuple3.compare(~f=TC.Int.compare, ~g=TC.Int.compare, ~h=TC.Int.compare)

  include Comparator.Make({
    type t = t
    let compare = compare
  })

  let add = ((a, b, c), (d, e, f)) => (a + d, b + e, c + f)
}

type grid = Grid(TC.Set.t<Coord.t, Coord.identity>)

let makeGrid = (lines: array<array<string>>) => {
  let createActive = (. x, y) => {
    lines->Array.getExn(y)->Array.getExn(x) === "#" ? Some((x, y, 0)) : None
  }
  let maxX = lines->Array.getExn(0)->Array.length - 1
  let maxY = lines->Array.length - 1
  let xs = Array.range(0, maxX)
  let ys = Array.range(0, maxY)
  combinationIfArray2(xs, ys, createActive)->TC.Set.fromArray(module(Coord))
}

let neighbourSpaces = (here: Coord.t) => {
  combinationIfArray3([-1, 0, 1], [-1, 0, 1], [-1, 0, 1], (. x, y, z) => {
    x == 0 && y == 0 && z == 0 ? None : Some(Coord.add((x, y, z), here))
  })->TC.Set.fromArray(module(Coord))
}

let countOccupiedNeighbours = (cell, Grid(grid)) => {
  cell->neighbourSpaces->TC.Set.intersection(grid)->TC.Set.length
}

let cubeSurvives = (Grid(grid), cell: Coord.t) => {
  let alive = grid->TC.Set.includes(cell)
  let nNbrs = countOccupiedNeighbours(cell, Grid(grid))
  alive && (nNbrs == 2 || nNbrs == 3)
}

let cubeBorn = (Grid(grid), cell) => {
  let dead = !(grid->TC.Set.includes(cell))
  let nNbrs = countOccupiedNeighbours(cell, Grid(grid))
  dead && nNbrs == 3
}

let update = (Grid(grid)) => {
  let mergeEmpties = (acc, cell) => {
    TC.Set.union(acc, neighbourSpaces(cell))
  }

  let empties =
    TC.Set.fold(grid, ~initial=TC.Set.empty(module(Coord)), ~f=mergeEmpties)->TC.Set.difference(
      grid,
    )

  TC.Set.union(
    grid->TC.Set.filter(~f=cubeSurvives(Grid(grid))),
    empties->TC.Set.filter(~f=cubeBorn(Grid(grid))),
  )
}

let parse = data => data->splitNewline->Array.map(compose(Js.String2.trim, splitChars))

let solvePart1 = data => {
  data->parse->log
  data->parse->makeGrid->TC.Set.toArray->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
