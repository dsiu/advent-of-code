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
    let keys =
      grid
      ->Map.keys
      ->Iterator.toArray
      ->Array.map(x => x->Coord_V2.fromString->Option.getExn)
    let rows = keys->Array.map(Tuple2.first)
    let cols = keys->Array.map(Tuple2.second)
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

  // todo: should move to Std Map
  let find: (grid, (key, 'a) => bool) => option<(key, 'a)> = (grid, f) => {
    grid->Map.entries->Iterator.toArray->Array.find(((k, v)) => f(k, v))
  }

  // todo: should move to Std Map
  let clone = Fn.compose(Map.entries, Map.fromIterator, _)

  let inRange: ((Coord_V2.t, Coord_V2.t), Coord_V2.t) => bool = ((min, max), c) => {
    let (r, c) = c
    let (minR, minC) = min
    let (maxR, maxC) = max
    r >= minR && r <= maxR && c >= minC && c <= maxC
  }

  let toString: grid => string = grid => {
    let bounds = grid->bounds
    let ((minR, minC), (maxR, maxC)) = bounds
    let rows = Array.fromInitializer(~length=maxR - minR + 1, i => {
      let cols = Array.fromInitializer(~length=maxC - minC + 1, j => {
        let c = (i + minR, j + minC)
        grid->Map.get(c->Coord_V2.toString)->Option.getExn->String.make
      })
      cols->Array.join("")
    })
    rows->Array.join("\n")
  }

  let walkAble: (grid, Coord_V2.t) => bool = (grid, c) => {
    let y = grid->Map.get(c->Coord_V2.toString)
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

let dirEq = (a, b) => {
  switch (a, b) {
  | (Up(_), Up(_)) => true
  | (Down(_), Down(_)) => true
  | (Left(_), Left(_)) => true
  | (Right(_), Right(_)) => true
  | _ => false
  }
}

type guard = {pos: position, dir: direction}
let guardEq = (a, b) => Coord_V2.compare(a.pos, b.pos) == 0 && dirEq(a.dir, b.dir)

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

let rec isLoop: (guard, array<guard>, CoordMap.t<'a>) => bool = (
  guard,
  trail,
  map,
) => {
  let stepped = step(map, guard)
  Option.isNone(stepped)
    ? false
    : {
        let (_, guard') = stepped->Option.getExn
        let hasTurned = guard.dir != guard'.dir
        let beenThere = trail->Array.find(guardEq(guard, _))->Option.isSome
        hasTurned && beenThere
          ? true
          : hasTurned
          ? {
            isLoop(guard', [guard, ...trail], map)
          }
          : isLoop(guard', trail, map)
      }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(l => l->String.trim->String.split("")->Array.map(MapValue.fromString))

let init = data => {
  let m: CoordMap.t<'a> = data->parse->CoordMap.fromArray
  let {grid, _} = m
  let guard = {
    pos: grid
    ->CoordMap.find((_k, v) => v == #"^")
    ->Option.map(((k, _v)) => k)
    ->Option.flatMap(Coord_V2.fromString)
    ->Option.getExn,
    dir: up,
  }
  (m, guard)
}

let solvePart1 = data => {
  let (m, guard) = init(data)

  let ret = walk(m, guard)->Array.uniq
  ret->Array.length
}

let solvePart2 = data => {
  let (m, guard) = init(data)
  let {grid, bounds} = m

  // excluding the starting point
  let news = walk(m, guard)->Array.uniq->Array.filter(x => Coord_V2.compare(x, guard.pos) != 0)

  // [ ] this is allocating too much memory and causes Node to crash with default heap size. need to optimize
  //  let modifiedGrids: array<CoordMap.t<'a>> = news->Array.map(g => {
  //    let m' = CoordMap.clone(grid)
  //    m'->Map.set(g->Coord_V2.toString, #"#")
  //    {CoordMap.grid: m', bounds}
  //  })

  //  modifiedGrids->Array.count(x => isLoop(guard, [], x))

  // this version fixed the heap issue but is still slow
  let gridsHaveLoop = news->Array.filter(g => {
    let m' = CoordMap.clone(grid)
    m'->Map.set(g->Coord_V2.toString, #"#")
    isLoop(guard, [], {CoordMap.grid: m', bounds})
  })

  gridsHaveLoop->Array.length
}
