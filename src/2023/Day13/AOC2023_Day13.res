@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type pattern = array<string>
type line = Horiz(int) | Vert(int)

let diffArray = (a, b) => {
  open Array
  zipWith(a, b, String.equal)->filter(x => !x->Fn.id)->length
}

let diffStrings = (a, b) => {
  open Array
  zipWith(a->String.split(""), b->String.split(""), String.equal)->filter(x => !x->Fn.id)->length
}

let reflectAt: (pattern, int, int) => bool = (xs, n, nDiff) => {
  open Array
  let (h, t) = xs->splitAt(~index=n)
  zipWith(h->toReversed, t, diffStrings)->sum(module(Int)) == nDiff
}

let reflectionLines: (pattern, int) => option<int> = (xs, nDiff) => {
  open Array
  //  log("reflectionLines")
  let k = xs->length
  //  k->(log2("k", _))
  // start with index 1 because we don't want to reflect at the whole pattern

  range(~from=1, k)->filter(x => reflectAt(xs, x, nDiff))->get(0)
}

let transposeArrayOfString = xs => {
  open Array
  let y = xs->map(String.split(_, ""))->transpose
  y->map(x => x->foldl1(String.concat))
}

let reflections: (pattern, int) => line = (patt, nDiff) => {
  open Option

  //  "=== try horiz ==="->log
  let hline = patt->reflectionLines(nDiff)->map(x => Horiz(x))
  //  "=== try vertical ==="->log
  let vline = patt->transposeArrayOfString->reflectionLines(nDiff)->map(x => Vert(x))
  hline->orElse(vline)->getExn
}

let score: line => int = l => {
  switch l {
  | Vert(x) => x
  | Horiz(x) => 100 * x
  }
}

let solve: (array<pattern>, int) => int = (xs, nDiff) => {
  xs
  ->Array.mapWithIndex((x, _i) => {
    //    i->(log2("i", _))
    x->reflections(nDiff)
  })
  ->Array.map(score)
  ->Array.sum(module(Int))
}

let part1 = xs => solve(xs, 0)
let part2 = xs => solve(xs, 1)

let parse = data =>
  data->splitDoubleNewline->Array.map(lines => lines->splitNewline->Array.map(String.trim))

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
