open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

/**
 * Parses the input data into a transposed array of integers.
 *
 * @param {string} data - The input data as a string.
 * @returns {array<array<int>>} The transposed array of integers.
 */
let parse = data =>
  data
  ->splitNewline
  ->Array.map(l =>
    l
    ->String.trim
    ->String.split("   ")
  )
  ->Array.map(l => [
    l->Array.get(0)->Option.flatMap(Int.fromString(_))->Option.getExn,
    l->Array.get(1)->Option.flatMap(Int.fromString(_))->Option.getExn,
  ])
  ->Array.transpose
let part1 = (l1, l2) => {
  Array.zipWith(l1->Array.toSorted(Int.compare), l2->Array.toSorted(Int.compare), (a, b) =>
    Math.Int.abs(a - b)
  )->Utils.sumIntArray
}

/**
 * Computes the sum of products of elements in l1 and their occurrences in l2.
 *
 * @param {array<int>} l1 - The first array of integers.
 * @param {array<int>} l2 - The second array of integers.
 * @returns {int} The sum of products of elements in l1 and their occurrences in l2.
 */
let part2 = (l1, l2) => {
  l1
  ->Array.map(a => a * l2->Array.filter(b => b == a)->Array.length)
  ->Utils.sumIntArray
}

let solvePart1 = data => {
  let [l1, l2] = data->parse
  let result = part1(l1, l2)
  result
}

let solvePart2 = data => {
  let [l1, l2] = data->parse
  let result = part2(l1, l2)
  result
}
