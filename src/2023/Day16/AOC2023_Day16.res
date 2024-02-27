open Stdlib
open Utils
module Map = Belt.Map
module Set = Belt.Set

let log = Console.log
let log2 = Console.log2

type element =
  | Empty
  | SlashMirror
  | BackslashMirror
  | HorizontalSplitter
  | VerticalSplitter

type position = Coord_V2.t

module Position = {
  type t = position
  let compare = Coord_V2.compare
  let cmp: (t, t) => int = (a, b) => compare(a, b)
}
module PositionCmp = Belt.Id.MakeComparableU(Position) // (r,c)

type grid = Map.t<PositionCmp.t, element, PositionCmp.identity>

type direction = U | D | L | R

type beamHead = BeamHead((position, direction))

module BeamHead = {
  type t = beamHead
  let cmp: (t, t) => int = (BeamHead((pos1, dir1)), BeamHead(pos2, dir2)) => {
    let posEq = Position.cmp(pos1, pos2)
    let dirEq = dir1 == dir2 ? 0 : 1
    posEq == 0 ? dirEq : posEq
  }
}
module BeamHeadCmp = Belt.Id.MakeComparableU(BeamHead)

//type energized = Set.t<beamHead>
type energized = Set.t<beamHead, BeamHeadCmp.identity>

let showBeamHead = (BeamHead((pos, dir))) => {
  let dirStr = switch dir {
  | U => "U"
  | D => "D"
  | L => "L"
  | R => "R"
  }
  `(${pos->Coord_V2.show}, ${dirStr})`
}

let bounds: grid => (int, int) = grid => {
  let rows = grid->Map.keysToArray->Array.map(((r, _)) => r)
  let cols = grid->Map.keysToArray->Array.map(((_, c)) => c)
  (
    rows->Array.reduce(Int.Constants.minValue, Math.Int.max),
    cols->Array.reduce(Int.Constants.minValue, Math.Int.max),
  )
}

let inRange = ((r1, c1), ~bounds as b) => {
  let (r2, c2) = b
  r1 >= 0 && r1 <= r2 && c1 >= 0 && c1 <= c2
}

let move = (pos, dir) => {
  module Dir = Coordinate.Direction
  switch dir {
  | U => Coord_V2.add(pos, (-1, 0))
  | D => Coord_V2.add(pos, (1, 0))
  | L => Coord_V2.add(pos, (0, -1))
  | R => Coord_V2.add(pos, (0, 1))
  }
}

let propagateElem: (element, beamHead) => array<beamHead> = (element, beamHead) => {
  switch (element, beamHead) {
  | (Empty, BeamHead(pos, dir)) => [BeamHead(move(pos, dir), dir)]
  | (SlashMirror, BeamHead(pos, L)) => [BeamHead(move(pos, D), D)]
  | (SlashMirror, BeamHead(pos, R)) => [BeamHead(move(pos, U), U)]
  | (SlashMirror, BeamHead(pos, U)) => [BeamHead(move(pos, R), R)]
  | (SlashMirror, BeamHead(pos, D)) => [BeamHead(move(pos, L), L)]
  | (BackslashMirror, BeamHead(pos, L)) => [BeamHead(move(pos, U), U)]
  | (BackslashMirror, BeamHead(pos, R)) => [BeamHead(move(pos, D), D)]
  | (BackslashMirror, BeamHead(pos, U)) => [BeamHead(move(pos, L), L)]
  | (BackslashMirror, BeamHead(pos, D)) => [BeamHead(move(pos, R), R)]
  | (HorizontalSplitter, BeamHead(pos, L)) => [BeamHead(move(pos, L), L)]
  | (HorizontalSplitter, BeamHead(pos, R)) => [BeamHead(move(pos, R), R)]
  | (HorizontalSplitter, BeamHead(pos, _)) => [BeamHead(move(pos, L), L), BeamHead(move(pos, R), R)]
  | (VerticalSplitter, BeamHead(pos, U)) => [BeamHead(move(pos, U), U)]
  | (VerticalSplitter, BeamHead(pos, D)) => [BeamHead(move(pos, D), D)]
  | (VerticalSplitter, BeamHead(pos, _)) => [BeamHead(move(pos, U), U), BeamHead(move(pos, D), D)]
  }
}

let rec propagate: (grid, (int, int), energized, array<beamHead>) => energized = (
  grid,
  bounds,
  energized,
  beamHeads,
) => {
  switch beamHeads {
  | [] => energized
  | _ => {
      let bh = beamHeads->Array.headUnsafe
      let bhs = beamHeads->Array.tail
      let BeamHead(beamPos, _) = bh

      switch energized->Set.has(bh) {
      | true => propagate(grid, bounds, energized, bhs)
      | false =>
        //          beamPos->(log2("beamPos", _))
        switch grid->Map.get(beamPos) {
        | Some(this) => {
            let nexts = propagateElem(this, bh)
            let nexts' = nexts->Array.filter((BeamHead(p, _)) => p->inRange(~bounds))
            let energized' = energized->Set.add(bh)
            propagate(grid, bounds, energized', Array.concat(bhs, nexts'))
          }
        | None => failwith("Beam head out of bounds")
        }
      }
    }
  }
}

let makeElement = s => {
  switch s {
  | "." => Empty
  | "/" => SlashMirror
  | "\\" => BackslashMirror
  | "-" => HorizontalSplitter
  | "|" => VerticalSplitter
  | _ => failwith("Unknown element")
  }
}

let makeGrid: array<array<string>> => grid = xss => {
  let r = xss->Array.length
  let c = xss->Array.getUnsafe(0)->Array.length
  Array.zipWith(Array.fromInitializer(~length=r, Fn.id), xss, (rowNum, row) => {
    Array.zipWith(Array.fromInitializer(~length=c, Fn.id), row, (colNum, col) => {
      ((rowNum, colNum), makeElement(col))
    })
  })
  ->Array.flatten
  ->Belt.Map.fromArray(~id=module(PositionCmp))
}

let countEnergized: (grid, (int, int), beamHead) => int = (grid, bounds, beamHead) => {
  propagate(grid, bounds, Set.make(~id=module(BeamHeadCmp)), [beamHead])
  ->Set.toArray
  ->Array.map((BeamHead(pos, _)) => pos)
  ->Array.uniq // todo: this IS the bottleneck.  How come there is no map in Set??
  ->Array.length
}

let getEdges: grid => array<beamHead> = grid => {
  let bounds = grid->bounds
  let (maxR, maxC) = bounds
  let top = Array.fromInitializer(~length=maxC + 1, c => BeamHead((0, c), D))
  let left = Array.fromInitializer(~length=maxR + 1, r => BeamHead((r, 0), R))
  let right = Array.fromInitializer(~length=maxR + 1, r => BeamHead((r, maxC), L))
  let bottom = Array.fromInitializer(~length=maxC + 1, c => BeamHead((maxR, c), U))
  [top, bottom, left, right]->Array.foldl1(Array.concat)
}

let part1 = xss => {
  let grid = xss->makeGrid
  let bounds = grid->bounds
  countEnergized(grid, bounds, BeamHead((0, 0), R))
}

let part2 = xss => {
  let grid = xss->makeGrid
  let bounds = grid->bounds
  let edges = grid->getEdges
  edges
  ->Array.map(countEnergized(grid, bounds, _))
  ->Array.maximum(~compare=Int.compare)
  ->Option.getExn
}

let parse = data => data->splitNewline->Array.map(l => l->String.trim->String.split(""))

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
