open Stdlib
open Utils
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
  open Array
  open String
  data
  ->splitNewline
  ->map(l => {
    l
    ->trim
    ->split(",")
    ->map(p => {
      let r = p->split("-")->map(intFromStringExn)
      Interval(r->getExn(0), r->getExn(1))
    })
  })
  ->map(l => {
    (l->getExn(0), l->getExn(1))
  })
}

let count = (xs, fn) => {
  open Array
  xs->keep(fn)->length
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
