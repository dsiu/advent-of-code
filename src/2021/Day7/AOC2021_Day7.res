open Belt
open Utils
let log = Js.Console.log

let median = xs => {
  let sorted = xs->SortArray.Int.stableSort
  let half = xs->Array.length / 2
  mod(xs->Array.length, 2) == 0
    ? sorted->Array.getExn(half)
    : (sorted->Array.getExn(half - 1) + sorted->Array.getExn(half)) / 2
}

let distance = (xs, m) => {
  xs->Array.reduce(0, (a, x) => a + Js.Math.abs_int(x - m))
}

// from point a to b
let cost = (a, b) => {
  Js.Math.abs_int(b - a) * (Js.Math.abs_int(b - a) + 1) / 2
}

let costAll = (xs, p) => {
  xs->Array.reduce(0, (a, x) => {
    a + cost(x, p)
  })
}

let parse = data =>
  data->Js.String2.trim->Js.String2.split(",")->Array.map(x => x->Int.fromString->Option.getExn)

let solvePart1 = data => {
  let xs = data->parse
  let m = xs->median
  xs->distance(m)
}

let solvePart2 = data => {
  let xs = data->parse
  let min = xs->minIntInArray
  let max = xs->maxIntInArray

  //  Js.log2("xs", xs)
  let result = Belt.Array.rangeBy(min, max, ~step=1)->Array.map(costAll(xs, _))->minIntInArray
  result
}
