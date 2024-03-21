open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type position = Coord_V2.t // (r,c)
type direction = U | D | L | R
type move = Move(direction, int)
type trail = array<move>
type directedPosition = (direction, position)
type grid = array<(position, int)>

module DirectedPosition = {
  type t = directedPosition

  let cmp: (t, t) => int = ((dir1, pos1), (dir2, pos2)) => {
    let posCmp = Coord_V2.compare(pos1, pos2)
    posCmp == 0
      ? switch (dir1, dir2) {
        | (U, U) => 0
        | (D, D) => 0
        | (L, L) => 0
        | (R, R) => 0
        | _ => -1
        }
      : posCmp
  }
}

module DirectedPositionCmp = Belt.Id.MakeComparableU(DirectedPosition)

type exploredStates = Set.t<directedPosition>

let delta: direction => position = dir => {
  switch dir {
  | U => (-1, 0)
  | D => (1, 0)
  | L => (0, -1)
  | R => (0, 1)
  }
}

let toPositions: (position, move) => array<position> = (here, Move(dir, n)) => {
  let d = delta(dir)

  Array.fromInitializer(~length=n - 1, i => i + 1)->Array.map(i => {
    here->Coord_V2.add(Coord_V2.mul(d, i))
  })
}

let endingDirPos: (position, move) => directedPosition = (here, Move(dir, _) as move) => {
  (dir, toPositions(here, move)->Array.last->Option.getExn)
}

let turnDirections: direction => array<direction> = dir => {
  switch dir {
  | U => [L, R]
  | D => [L, R]
  | L => [U, D]
  | R => [U, D]
  }
}

let parse = data => data->splitNewline->Array.map(String.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
