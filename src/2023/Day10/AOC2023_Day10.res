@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t // (x,y) (col, row)

@unboxed
type pipe = Empty | NW | NS | NE | WE | WS | SE | Start

let mkPipe = char => {
  switch char {
  | "J" => NW
  | "|" => NS
  | "L" => NE
  | "-" => WE
  | "7" => WS
  | "F" => SE
  | "S" => Start
  | _ => Empty
  }
}

let rec deltas: pipe => array<position> = pipe => {
  let north = (0, -1)
  let south = (0, 1)
  let east = (1, 0)
  let west = (-1, 0)

  switch pipe {
  | NW => [north, west]
  | NS => [north, south]
  | NE => [north, east]
  | WE => [west, east]
  | WS => [west, south]
  | SE => [south, east]
  | Start => Array.concat(deltas(NS), deltas(WE))
  | Empty => []
  }
}

let isVertex: pipe => bool = pipe => {
  switch pipe {
  | NW | NE | WS | SE | Start => true
  | _ => false
  }
}

type grid = Array2D.t<pipe>

let findStart: grid => position = grid => {
  grid->Array2D.reduceWithIndex((0, 0), (acc, elem, pos) => {
    switch elem {
    | Start => pos
    | _ => acc
    }
  })
}

module Map = {
  type t = {grid: grid, start: position}
  let make: grid => t = grid => {
    {grid, start: grid->findStart}
  }
}

let neighbours: (Map.t, position) => array<position> = ({grid, start}, p) => {
  grid
  ->Array2D.getExn(p)
  ->deltas
  ->Array.filterMap(delta => {
    let nbr = Coord_V2.add(p, delta)
    grid->Array2D.isValidXY(nbr) ? Some(nbr) : None
  })
}

// find connectors that connections to a location
let connectorsToPosition: (Map.t, position) => array<position> = ({grid, start} as map, pos) => {
  let startNbrs = neighbours(map, pos)

  let nbrsNbrs = startNbrs->Array.map(nbr => (nbr, neighbours(map, nbr)))
  let connectors = nbrsNbrs->Array.filter(((snbr, nbr)) => {
    nbr
    ->Array.find(n => {
      Coord_V2.compare(n, pos) == 0
    })
    ->Option.isSome
  })

  connectors->Array.map(fst)
}

let connectorsToStart: Map.t => array<position> = ({grid, start} as map) => {
  connectorsToPosition(map, start)
}

// find path from start to end
let followPath: (Map.t, position, position) => array<position> = (
  {grid, start} as map,
  start,
  end,
) => {
  let rec loop: (array<position>, position, position) => array<position> = (
    acc,
    thisPos,
    lastPos,
  ) => {
    let nbrs = connectorsToPosition(map, thisPos)
    // nbrs must contains current position and next position.  remove current position and
    // proceed with the next position

    let next = nbrs->Array.filter(n => Coord_V2.compare(n, lastPos) != 0)->Array.getUnsafe(0)

    let acc' = Array.concat(acc, [thisPos])
    Coord_V2.compare(next, start) == 0 ? acc' : loop(acc', next, thisPos)
  }

  loop([], start, start)
}

let part1: Map.t => int = map => {
  let path = map->followPath(map.start, map.start)
  path->Array.length / 2
}

/**
 * ref: https://github.com/tlareg/advent-of-code/blob/master/src/2023/day10/index.ts
 *
 * Pick's theorem (https://en.wikipedia.org/wiki/Pick%27s_theorem)
 * loopArea = interiorPointsCount + (boundaryPointsCount / 2) - 1
 *
 * Part 2 answer is interiorPointsCount
 * transforming Pick's formula:
 * interiorPointsCount = loopArea - (boundaryPointsCount / 2) + 1
 *
 * boundaryPointsCount is length of loop (practically part1 answer * 2)
 *
 * loopArea can by calculated using Shoelace formula (https://en.wikipedia.org/wiki/Shoelace_formula):
 * vertices = (x1, y1) (x2, y2) (x3, y3) ...
 * 2 * loopArea = x1 * y2 - y1 * x2 + x2 * y3 - x3 * y2 + ...
 * loopArea = result / 2
*/
let shoelaceFormula: array<position> => int = v => {
  open Array
  let v' = concat(v->drop(1), v->take(1))

  map2(v, v', ~f=((x1, y1), (x2, y2)) => {
    x1 * y2 - y1 * x2
  })
  ->sum(module(Int))
  ->Math.Int.abs / 2
}

let part2: Map.t => int = ({grid, start} as map) => {
  let path = map->followPath(start, start)
  let boundaryPointsCount = path->Array.length
  let vertices = path->Array.filter(pos => map.grid->Array2D.getExn(pos)->isVertex)
  let loopArea = vertices->shoelaceFormula

  loopArea - boundaryPointsCount / 2 + 1
}

let parse: string => grid = data =>
  data->splitNewline->Array.map(x => x->String.trim->String.split("")->Array.map(mkPipe))

let solvePart1 = data => {
  let map = data->parse->Map.make
  map->part1
}

let solvePart2 = data => {
  let map = data->parse->Map.make
  map->part2
}
