open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type element =
  | Empty
  | SlashMirror
  | BackslashMirror
  | HorizontalSplitter
  | VerticalSplitter

type position = Coord_V2.t // (row,col)
type direction = U | D | L | R

type grid = Map.t<position, element>
type beamHead = BeamHead((position, direction))
type energized = Set.t<beamHead>

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

let propagate: (grid, energized, array<beamHead>) => energized = (grid, energized, beanHeads) => {
  energized
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
  ->Map.fromArray
}

let parse = data => data->splitNewline->Array.map(l => l->String.trim->String.split(""))

let solvePart1 = data => {
  let p = data->parse
  p->log

  p->makeGrid->log

  1
}

let solvePart2 = data => {
  data->ignore
  2
}
