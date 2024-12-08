open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t

module CoordMap = {
  type key = string
  type t<'a> = Map.t<key, 'a>

  let fromArray: array<array<'a>> => t<'a> = arr => {
    arr
    ->Array.mapWithIndex((row, i) => {
      row->Array.mapWithIndex((col, j) => {
        ((i, j)->Coord_V2.toString, col)
      })
    })
    ->Array.flat
    ->Map.fromArray
  }

  let findValueWithKey: (t<'a>, (key, 'a) => bool) => option<key> = (map, f) => {
    map->Map.entries->Iterator.toArray->Array.find(((k, v)) => f(k, v))->Option.map(((k, v)) => k)
  }

  // todo: should refactor bounds - it was used in multiple places
  let bounds: t<'a> => (Coord_V2.t, Coord_V2.t) = map => {
    let keys = map->Map.keys->Iterator.toArray->Array.map(Coord_V2.fromString)
    let rows = keys->Array.map(r => r->Option.getUnsafe->Tuple2.first)
    let cols = keys->Array.map(c => c->Option.getUnsafe->Tuple2.second)
    ((rows->minIntInArray, cols->minIntInArray), (rows->maxIntInArray, cols->maxIntInArray))
  }

  let inRange: ((Coord_V2.t, Coord_V2.t), Coord_V2.t) => bool = ((min, max), c) => {
    let (r, c) = c
    let (minR, minC) = min
    let (maxR, maxC) = max
    r >= minR && r <= maxR && c >= minC && c <= maxC
  }
}

module MapValue = {
  type t = [#"#" | #"." | #"^"]
  let fromString: string => t = s => {
    switch s {
    | "#" => #"#"
    | "." => #"."
    | "^" => #"^"
    | _ => failwith("Invalid value")
    }
  }
}

type grid = CoordMap.t<MapValue.t>

type direction = Up(position) | Down(position) | Left(position) | Right(position)

let up = Up((-1, 0))
let down = Down((1, 0))
let left = Left((0, -1))
let right = Right((0, 1))

let turnRight: direction => direction = dir => {
  switch dir {
  | Up(_) => right
  | Right(_) => down
  | Down(_) => left
  | Left(_) => up
  }
}

type guard = {pos: position, dir: direction}

let rec step: (grid, guard) => option<(position, guard)> = (grid, guard) => {
  None
}

let walk: (grid, guard) => array<position> = (grid, guard) => {
  Array.unfoldr(guard, step(grid, _))
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => l->String.trim->String.split("")->Array.map(MapValue.fromString))

let solvePart1 = data => {
  let grid: grid = data->parse->CoordMap.fromArray
  let guard = (grid->CoordMap.findValueWithKey((k, v) => v == #"^"), up)
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
