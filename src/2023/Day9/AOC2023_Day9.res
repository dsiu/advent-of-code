@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type sequence = Sequence(array<array<int>>)

// let rec expand: sequence => sequence = (Sequence(xss)) => {}

let differences: array<int> => array<int> = xs => {
  open Array
  map2(tail(xs), xs, ~f=Int.subtract)
}

let expand: array<int> => sequence = seq => {
  open Array
  seq
  ->unfoldr(xs => {
    xs->all(Fn.eq(0)) ? None : Some(xs, differences(xs))
  })
  ->Sequence
}

let extendRow: ((array<array<int>>, int), array<int>) => (array<array<int>>, int) = (
  (seq, n),
  row,
) => {
  open Array
  let n' = lastUnsafe(row) + n
  let row' = concat(row, [n'])
  (concat([row'], seq), n')
}

let extend: sequence => sequence = (Sequence(seq)) => {
  open Array
  seq
  ->foldRight(~initial=([], 0), ~f=extendRow)
  ->fst
  ->Sequence
}

let evaluate: sequence => int = (Sequence(seq)) => {
  open Array
  seq->headUnsafe->lastUnsafe
}

let part1: array<array<int>> => int = histories => {
  open Array
  histories
  ->map(x => x->expand->extend->evaluate)
  ->sum(module(Int))
}

let part2: array<array<int>> => int = histories => {
  open Array
  histories->map(toReversed)->part1
}

let parse = data => {
  open Array
  data
  ->splitNewline
  ->map(l => {
    l
    ->String.trim
    ->String.split(" ")
    ->map(Fn.compose(Int.fromString, Option.getExn))
  })
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
