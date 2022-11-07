open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

module TC = Tablecloth

module Location = Coord_V2

let manhattan = ((x, y)) => {Js.Math.abs_int(x) + Js.Math.abs_int(y)}

type visited = TC.Set.t<Location.t, Location.identity>
let emptyVisited = TC.Set.empty(module(Location))

type direction = East | South | West | North

type segment = {
  direction: direction,
  steps: int,
}

exception InvalidDirection(string)

let makeSegment = str => {
  let direction = str->Js.String2.get(0)
  let steps = str->Js.String2.sliceToEnd(~from=1)->Int.fromString->Option.getExn

  switch direction {
  | "R" => {direction: East, steps}
  | "D" => {direction: South, steps}
  | "L" => {direction: West, steps}
  | "U" => {direction: North, steps}
  | _ => raise(InvalidDirection(direction))
  }
}

let facing = (direction: direction): Location.t => {
  switch direction {
  | East => (1, 0)
  | South => (0, -1)
  | West => (-1, 0)
  | North => (0, 1)
  }
}

type path = {
  visited: visited,
  tip: Location.t,
}

let travelSegment = (path, segment) => {
  open FP_Utils
  //  path->log2("path", _)
  //  segment->log2("segment", _)
  let delta = facing(segment.direction)
  //  delta->log2("delta", _)
  let distance = segment.steps
  let start = path.tip
  let visited = path.visited

  let visited' =
    unfold(
      ((a, _x)) => a >= distance,
      ((a, x)) => (Location.add(x, delta), (a + 1, Location.add(x, delta))),
      (0, start),
    )->TC.List.fold(~initial=visited, ~f=TC.Set.add)

  let tip' = Location.add(start, Location.mul(delta, distance))
  {tip: tip', visited: visited'}
}

let travelPath = segments => {
  let path0 = {visited: emptyVisited, tip: (0, 0)}
  segments->TC.Array.fold(~initial=path0, ~f=travelSegment)
}

let travelAllPaths = Array.map(_, travelPath)

let closest = points => {
  points->Set.toArray->Array.map(manhattan)->minIntInArray
}

let crossovers = travelledPaths => {
  open FP_Utils
  travelledPaths->Array.map(({visited}) => visited)->foldLeftArray(TC.Set.intersection)
}

let part1 = segmentss => {
  segmentss->travelAllPaths->crossovers->closest
}

let parse = data => data->splitNewline->Array.map(x => x->Js.String2.trim->Js.String2.split(","))

let solvePart1 = data => {
  let segs = data->parse->Array.map(Array.map(_, makeSegment))
  segs->part1
}

let solvePart2 = data => {
  data->ignore
  2
}
