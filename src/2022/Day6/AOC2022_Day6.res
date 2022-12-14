open Stdlib
open Utils
let log = Js.Console.log

module A = Array

let parse = data => data->splitChars

let allDifferent = cs => {
  A.uniq(cs)->A.eq(cs, \"==")
}

let hasSome = x => {!allDifferent(x)}

let interestingPosition = (n, text) => {
  let len = text->A.length
  let candidates = zip(A.makeby)
  n + fst(packetPos)
}

let solvePart1 = data => {
  interestingPosition(4, data->parse)->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
