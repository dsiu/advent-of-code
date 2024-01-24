@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type pattern = array<string>
type line = Horiz(int) | Vert(int)

// todo: add to stdlib
let zipWith = (a, b, f) => Array.map2(a, b, ~f)

let reflectAt: (pattern, int) => bool = (xs, n) => {
  open Array
  let (h, t) = xs->splitAt(~index=n)
  //  log("reflectAt")
  //  n->(log2("n", _))
  //  h->(log2("h", _))
  //  t->(log2("t", _))
  let z = zipWith(h->toReversed, t, String.equal)
  //  z->(log2("z", _))
  zipWith(h->toReversed, t, String.equal)->all(Fn.id)
}

let reflectionLines: pattern => option<int> = xs => {
  open Array
  //  log("reflectionLines")
  let k = xs->length
  //  k->(log2("k", _))
  // start with index 1 because we don't want to reflect at the whole pattern
  range(~from=1, k)->filter(x => reflectAt(xs, x))->get(0)
}

let transposeArrayOfString = xs => {
  open Array
  let y = xs->map(String.split(_, ""))->transpose
  y->map(x => x->foldl1(String.concat))
}

let reflections: pattern => line = patt => {
  open Option

  //  "=== try horiz ==="->log
  let hline = patt->reflectionLines->map(x => Horiz(x))
  //  "=== try vertical ==="->log
  let vline = patt->transposeArrayOfString->reflectionLines->map(x => Vert(x))
  hline->orElse(vline)->getExn
}

let score: line => int = l => {
  switch l {
  | Vert(x) => x
  | Horiz(x) => 100 * x
  }
}

let part1: array<pattern> => int = xs => {
  xs
  ->Array.mapWithIndex((x, i) => {
    //    i->(log2("i", _))
    x->reflections
  })
  ->Array.map(score)
  ->Array.sum(module(Int))
}

let parse = data =>
  data->splitDoubleNewline->Array.map(lines => lines->splitNewline->Array.map(String.trim))

let solvePart1 = data => {
  data->parse->part1->log
  //  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
