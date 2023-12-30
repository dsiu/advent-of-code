// ref: https://work.njae.me.uk/2020/12/26/advent-of-code-2020-day-17/

open Belt
open Utils

let log = Js.Console.log

module TC = Tablecloth

module Coord = {
  open TC
  type t =
    | Coord_V3(Coord_V3.t)
    | Coord_V4(Coord_V4.t)

  let compare = (a, b) => {
    switch (a, b) {
    | (Coord_V3(a), Coord_V3(b)) => Coord_V3.compare(a, b)
    | (Coord_V4(a), Coord_V4(b)) => Coord_V4.compare(a, b)
    | (_, _) => failwith("Invalid comparison")
    }
  }

  include Comparator.Make({
    type t = t
    let compare = compare
  })

  let add = (a, b) => {
    switch (a, b) {
    | (Coord_V3(a), Coord_V3(b)) => Coord_V3(Coord_V3.add(a, b))
    | (Coord_V4(a), Coord_V4(b)) => Coord_V4(Coord_V4.add(a, b))
    | (_, _) => failwith("Invalid addition")
    }
  }
}

type grid = TC.Set.t<Coord.t, Coord.identity>

let makeGrid = (lines: array<array<string>>) => {
  let createActive = (x, y) => {
    lines->Array.getExn(y)->Array.getExn(x) === "#" ? Some(Coord.Coord_V3(x, y, 0)) : None
  }
  let maxX = lines->Array.getExn(0)->Array.length - 1
  let maxY = lines->Array.length - 1
  let xs = Array.range(0, maxX)
  let ys = Array.range(0, maxY)
  Stdlib.Array.combinationIf2(xs, ys, createActive)->TC.Set.fromArray(module(Coord))
}

@@warning("-8")
let conv34Cell = (Coord.Coord_V3(x, y, z)): Coord.t => Coord.Coord_V4(x, y, z, 0)

let conv34 = grid => {
  grid->TC.Set.toArray->TC.Array.map(conv34Cell)->TC.Set.fromArray(module(Coord))
}

let neighbourSpaces = (here: Coord.t) => {
  open Stdlib.Array
  switch here {
  | Coord_V3(_) as here =>
    combinationIf3([-1, 0, 1], [-1, 0, 1], [-1, 0, 1], (x, y, z) => {
      x == 0 && y == 0 && z == 0 ? None : Some(Coord.add(Coord.Coord_V3(x, y, z), here))
    })->TC.Set.fromArray(module(Coord))
  | Coord_V4(_) as here =>
    combinationIf4([-1, 0, 1], [-1, 0, 1], [-1, 0, 1], [-1, 0, 1], (x, y, z, w) => {
      x == 0 && y == 0 && z == 0 && w == 0
        ? None
        : Some(Coord.add(Coord.Coord_V4(x, y, z, w), here))
    })->TC.Set.fromArray(module(Coord))
  }
}

let countOccupiedNeighbours = (cell, grid) => {
  cell->neighbourSpaces->TC.Set.intersection(grid)->TC.Set.length
}

let cubeSurvives = (grid, cell: Coord.t) => {
  let alive = grid->TC.Set.includes(cell)
  let nNbrs = countOccupiedNeighbours(cell, grid)
  alive && (nNbrs == 2 || nNbrs == 3)
}

let cubeBorn = (grid, cell) => {
  let dead = !(grid->TC.Set.includes(cell))
  let nNbrs = countOccupiedNeighbours(cell, grid)
  dead && nNbrs == 3
}

let update = grid => {
  let mergeEmpties = (acc, cell) => {
    TC.Set.union(acc, neighbourSpaces(cell))
  }

  let empties =
    TC.Set.fold(grid, ~initial=TC.Set.empty(module(Coord)), ~f=mergeEmpties)->TC.Set.difference(
      grid,
    )

  TC.Set.union(
    grid->TC.Set.filter(~f=cubeSurvives(grid)),
    empties->TC.Set.filter(~f=cubeBorn(grid)),
  )
}

let rec iterate = (grid, f, times) => {
  times == 0 ? grid : iterate(f(grid), f, times - 1)
}

let parse = data => data->splitNewline->Array.map(compose(Js.String2.trim, splitChars))

let solvePart1 = data => {
  //  data->parse->log
  let grid0 = data->parse->makeGrid
  let finalGrid = iterate(grid0, update, 6)
  TC.Set.length(finalGrid)
}

let solvePart2 = data => {
  let grid = data->parse->makeGrid->conv34
  let finalGrid = iterate(grid, update, 6)
  TC.Set.length(finalGrid)
}
