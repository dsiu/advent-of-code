open Stdlib
open Utils
let log = Js.Console.log

module A = Stdlib.Array
type operation =
  | Noop
  | Addx(int)

type signal = (int, int)

let applyOp: operation => array<int> = op => {
  switch op {
  | Noop => [0]
  | Addx(d) => [0, d]
  }
}

let apply: array<operation> => array<signal> = ops => {
  let len = ops->A.length * 2 // Applyop returns an array of length 2 at most
  A.zip(A.makeBy(len, x => x + 1), ops->A.flatMap(applyOp)->A.scanl(1, \"+"))
}

let extractSignals: array<signal> => array<signal> = signals => {
  signals->A.keep(((t, _n)) => mod(t + 20, 40) == 0)
}

let calculateSixSignals: array<signal> => int = signals => {
  signals->A.keepMap(((t, n)) => {t <= 220 ? Some(t * n) : None})->sumIntArray
}

let part1: array<signal> => int = compose(extractSignals, calculateSixSignals)

let showPixel: bool => string = b =>
  switch b {
  | true => "█"
  | false => " "
  }

let columnOf: int => int = n => {
  mod(n - 1, 40)
}

let isLit: signal => bool = ((n, x)) => {
  let c = columnOf(n)
  abs(x - c) <= 1
}

let part2 = regVals => {
  let pixels = regVals->A.map(compose(isLit, showPixel))
  pixels->JsArray2Ex.chunkBySize(40)->A.map(x => x->A.joinWith("", Function.identity))
}

@@warning("-8")
let parse = data => {
  data
  ->splitNewline
  ->Array.map(x => {
    let l = x->Js.String2.trim
    l->String.startsWith("noop")
      ? Noop
      : {
          let [_a, v] = l->String.split(" ")
          Addx(v->int_of_string)
        }
  })
}

let solvePart1 = data => {
  data->parse->apply->part1
}

let solvePart2 = data => {
  data->parse->apply->part2->log
  "EFGERURE"
}
