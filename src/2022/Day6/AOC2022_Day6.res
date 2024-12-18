open Stdlib
open Utils
let log = Js.Console.log

module A = Array

let parse = data => data->splitChars

let allDifferent = cs => {
  A.uniq(cs)->A.equal(cs, \"==")
}

let hasSame = x => {!allDifferent(x)}

let interestingPosition: (int, array<string>) => int = (n, text) => {
  let len = text->A.length
  let candidates = A.zip(A.makeBy(len, Fn.identity), text->A.tails->A.map(A.take(_, n)))
  let packetPos = candidates->A.dropWhile(compose(snd, hasSame))->A.headUnsafe
  n + fst(packetPos)
}

let solvePart1 = data => {
  interestingPosition(4, data->parse)
}

let solvePart2 = data => {
  interestingPosition(14, data->parse)
}
