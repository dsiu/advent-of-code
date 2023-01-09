open Stdlib
open Utils
module A = Array
module S = String

let log = Js.Console.log

type interval = Interval(int, int)

let contains = (Interval(a, b), Interval(c, d)) => {
  a <= c && b >= d
}

let before = (Interval(_a, b), Interval(c, _d)) => {
  b < c
}

let hasContainment = (x, y) => {
  contains(x, y) || contains(y, x)
}

let disjoint = (x, y) => {
  before(x, y) || before(y, x)
}

let overlaps = (x, y) => !disjoint(x, y)

let parse = data => {
  data
  ->splitNewline
  ->A.map(l => {
    l
    ->S.trim
    ->S.split(",")
    ->A.map(p => {
      let r = p->S.split("-")->A.map(intFromStringExn)
      Interval(r->A.getExn(0), r->A.getExn(1))
    })
  })
  ->A.map(l => {
    (l->A.getExn(0), l->A.getExn(1))
  })
}

let count = (xs, fn) => {
  xs->A.keep(fn)->A.length
}

let part1 = xs => {
  xs->count(((a, b)) => {
    hasContainment(a, b)
  })
}

let part2 = xs => {
  xs->count(((a, b)) => {
    overlaps(a, b)
  })
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
