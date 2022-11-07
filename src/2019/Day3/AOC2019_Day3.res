open Belt
open Utils
let log = Js.Console.log

module TC = Tablecloth

module Location = Coord_V2

type visited = TC.Set.t<Location.t, Location.identity>

type direction = East | South | West | North

type segment = {
  direction: direction,
  steps: int,
}

let makeSegment = str => {
  let direction = str->Js.String2.get(0)
  let steps = str->Js.String2.sliceToEnd(~from=1)->Int.fromString->Option.getExn

  switch direction {
  | "R" => {direction: East, steps}
  | "D" => {direction: South, steps}
  | "L" => {direction: West, steps}
  | "U" => {direction: North, steps}
  | _ => failwith("Invalid direction")
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

let parse = data => data->splitNewline->Array.map(x => x->Js.String2.trim->Js.String2.split(","))

let solvePart1 = data => {
  let segs = data->parse->Array.map(Array.map(_, makeSegment))
  segs->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
