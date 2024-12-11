open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t // row, col

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

module CoordMap = {
  type key = string
  type grid = Map.t<key, MapValue.t>

  type t<'a> = {grid: grid, bounds: (Coord_V2.t, Coord_V2.t)}

  // todo: should refactor bounds - it was used in multiple places
  let bounds: grid => (Coord_V2.t, Coord_V2.t) = grid => {
    let keys = grid->Map.keys->Iterator.toArray->Array.map(Coord_V2.fromString)
    let rows = keys->Array.map(r => r->Option.getUnsafe->Tuple2.first)
    let cols = keys->Array.map(c => c->Option.getUnsafe->Tuple2.second)
    ((rows->minIntInArray, cols->minIntInArray), (rows->maxIntInArray, cols->maxIntInArray))
  }

  let fromArray: array<array<'a>> => t<'a> = arr => {
    let grid =
      arr
      ->Array.mapWithIndex((row, i) => {
        row->Array.mapWithIndex((col, j) => {
          ((i, j)->Coord_V2.toString, col)
        })
      })
      ->Array.flat
      ->Map.fromArray
    let bounds = grid->bounds
    {grid, bounds}
  }

  // should move to Std Map
  let find: (grid, (key, 'a) => bool) => option<(key, 'a)> = (grid, f) => {
    grid->Map.entries->Iterator.toArray->Array.find(((k, v)) => f(k, v))
  }

  let inRange: ((Coord_V2.t, Coord_V2.t), Coord_V2.t) => bool = ((min, max), c) => {
    let (r, c) = c
    let (minR, minC) = min
    let (maxR, maxC) = max
    r >= minR && r <= maxR && c >= minC && c <= maxC
  }

  let walkAble: (grid, Coord_V2.t) => bool = (grid, c) => {
    let y = find(grid, (k, v) => {
      k == c->Coord_V2.toString
    })->Option.map(Tuple2.second)
    switch y {
    | Some(#"#") => false
    | Some(_)
    | None => true
    }
  }
}

type direction = Up(position) | Down(position) | Left(position) | Right(position)

let up = Up((-1, 0))
let down = Down((1, 0))
let left = Left((0, -1))
let right = Right((0, 1))

let delta = dir => {
  switch dir {
  | Up(p) => p
  | Down(p) => p
  | Left(p) => p
  | Right(p) => p
  }
}

let turnRight: direction => direction = dir => {
  switch dir {
  | Up(_) => right
  | Right(_) => down
  | Down(_) => left
  | Left(_) => up
  }
}

type guard = {pos: position, dir: direction}

let step: (CoordMap.t<'a>, guard) => option<(position, guard)> = ({grid, bounds}, guard) => {
  let ahead = Coord_V2.add(guard.pos, guard.dir->delta)

  !CoordMap.inRange(bounds, guard.pos)
    ? None
    : !CoordMap.inRange(bounds, ahead)
    ? Some(guard.pos, {...guard, pos: ahead})
    : !CoordMap.walkAble(grid, ahead)
    ? Some(guard.pos, {...guard, dir: guard.dir->turnRight})
    : Some(guard.pos, {...guard, pos: ahead})
}

let walk: (CoordMap.t<'a>, guard) => array<position> = (map, guard) => {
  Array.unfoldr(guard, step(map, _))
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => l->String.trim->String.split("")->Array.map(MapValue.fromString))

let solvePart1 = data => {
  let m: CoordMap.t<'a> = data->parse->CoordMap.fromArray
  let {grid, bounds} = m
  let guard = {
    pos: grid
    ->CoordMap.find((k, v) => v == #"^")
    ->Option.map(((k, v)) => k)
    ->Option.flatMap(Coord_V2.fromString)
    ->Option.getUnsafe,
    dir: up,
  }
  let ret = walk(m, guard)
  let ret' = ret->Array.uniq
  //  ret->log
  //  ret'->log
  ret'->Array.length
}

let solvePart2 = data => {
  data->ignore
  2
}
