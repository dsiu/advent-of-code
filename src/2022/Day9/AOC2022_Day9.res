open Stdlib
open Utils
let log = Js.Console.log

module A = Array
module TC = Tablecloth

module Position = Coord_V2
type position = Position.t
type trace = TC.Set.t<Position.t, Position.identity>
type path = array<position>

let emptyPositionSet = TC.Set.empty(module(Position))

type rope = Rope({headK: position, knots: array<position>, trace: trace})

type direction = U(int) | R(int) | D(int) | L(int)

let newRope: int => rope = n => Rope({
  headK: (0, 0),
  knots: Array.make(~length=n, (0, 0)),
  trace: emptyPositionSet->TC.Set.add((0, 0)),
})

exception ParseError(string)

let expandPath: array<direction> => path = directions => {
  let expandStep = step =>
    switch step {
    | U(n) => Array.make(~length=n, (0, 1))
    | L(n) => Array.make(~length=n, (-1, 0))
    | D(n) => Array.make(~length=n, (0, -1))
    | R(n) => Array.make(~length=n, (1, 0))
    }

  directions->A.flatMap(expandStep)
}

let manhattan: (position, position) => int = (p1, p2) => {
  let (x1, y1) = p1
  let (x2, y2) = p2
  max(abs(x1 - x2), abs(y1 - y2))
}

let touching: (position, position) => bool = (p1, p2) => {
  manhattan(p1, p2) <= 1
}

let sign = n => compare(n, 0)

let towards: (position, position) => position = (p1, p2) => {
  let (x1, y1) = p1
  let (x2, y2) = p2
  (sign(x2 - x1), sign(y2 - y1))
}

let knotStep: ((position, array<position>), position) => (position, array<position>) = (
  (h, ks),
  kt,
) => {
  let kt' = {
    kt->touching(h) ? kt : kt->Position.add(kt->towards(h))
  }
  (kt', A.concat([kt'], ks))
}

let ropeStep: (rope, position) => rope = (rope, step) => {
  let Rope({headK, knots, trace}) = rope
  let h = headK->Position.add(step)
  let (kt, kts) = knots->A.reduce((h, []), knotStep)

  Rope({
    headK: h,
    knots: A.toReversed(kts),
    trace: trace->TC.Set.add(kt),
  })
}

let ropeSteps: (rope, path) => rope = (rope, steps) => {
  steps->A.reduce(rope, ropeStep)
}

let part1: path => int = steps => {
  let Rope(rope) = ropeSteps(newRope(1), steps)
  rope.trace->TC.Set.length
}

let part2: path => int = steps => {
  let Rope(rope) = ropeSteps(newRope(9), steps)
  rope.trace->TC.Set.length
}

@@warning("-8")
let parse = data => {
  module S = String
  data
  ->splitNewline
  ->Array.map(x => {
    let [dStr, steps] = x->S.trim->S.split(" ")
    switch (dStr, steps->Int.fromString->Option.getExn) {
    | ("U", s) => U(s)
    | ("L", s) => L(s)
    | ("D", s) => D(s)
    | ("R", s) => R(s)
    | _ => raise(ParseError(dStr ++ steps))
    }
  })
}

let solvePart1 = data => {
  data->parse->expandPath->part1
}

let solvePart2 = data => {
  data->parse->expandPath->part2
}
