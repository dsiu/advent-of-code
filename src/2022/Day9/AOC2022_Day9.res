open Stdlib
open Utils
let log = Js.Console.log

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
  knots: Array.make(n, (0, 0)),
  trace: emptyPositionSet->TC.Set.add((0, 0)),
})

exception ParseError(string)

let parse = data => {
  module S = String
  data
  ->splitNewline
  ->Array.map(x => {
    let [dStr, steps] = x->S.trim->S.split(" ")
    switch (dStr, steps->int_of_string) {
    | ("U", s) => U(s)
    | ("L", s) => L(s)
    | ("D", s) => D(s)
    | ("R", s) => R(s)
    | _ => raise(ParseError(dStr ++ steps))
    }
  })
}

let solvePart1 = data => {
  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
