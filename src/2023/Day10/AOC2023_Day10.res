@@uncurried

open Stdlib
open Utils
let log = Console.log

type position = Coord_V2.t // (x,y) (col, row)

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

let neighbours: (Map.t, position) => array<position> = ({grid, start}, p) => {
  grid
  ->Array2D.getExn(p)
  ->deltas
  ->Array.filterMap(delta => {
    let nbr = Coord_V2.add(p, delta)
    grid->Array2D.isValidXY(nbr) ? Some(nbr) : None
  })
}

let connectorsToStart: Map.t => array<position> = ({grid, start} as map) => {
  let nbrs = neighbours(map, map.start)
  let nbrsNbrs = nbrs->Array.map(nbr => (nbr, neighbours(map, nbr)))
  let connectors = nbrsNbrs->Array.filter(((snbr, nbr)) => {
    nbr
    ->Array.find(n => {
      Coord_V2.compare(n, snbr) == 0
    })
    ->Option.isSome
  })

  connectors->Array.map(fst)
}

let parse: string => grid = data =>
  data->splitNewline->Array.map(x => x->String.trim->String.split("")->Array.map(mkPipe))

let solvePart1 = data => {
  let map = data->parse->Map.make
  map->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
