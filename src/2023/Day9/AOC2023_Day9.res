@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type sequence = Sequence(array<array<int>>)

/**
 * This function calculates the differences between consecutive elements in an array.
 * It takes an array of integers as input and returns an array of integers as output.
 * The output array is one element shorter than the input array, as it represents the differences between consecutive elements.
 *
 * @param xs - The input array of integers.
 * @returns An array of integers representing the differences between consecutive elements in the input array.
 *
 * @example
 * differences([5, 2, 9]) // returns [-3, 7]
 * differences([1, 1, 1, 1]) // returns [0, 0, 0]
 */
let differences: array<int> => array<int> = xs => {
  open Array
  map2(tail(xs), xs, ~f=Int.subtract)
}

/**
 * This function expands a sequence by applying the differences function until all elements are zero.
 * It takes an array of integers as input and returns a sequence.
 * The sequence is a type alias for an array of arrays of integers.
 *
 * The function stops expanding the sequence when all elements in the array are zero.
 * The returned sequence does not include the array with all zeros.
 *
 * @param seq - The input array of integers.
 * @returns A sequence where each array is the result of applying the differences function to the previous array, until all elements are zero.
 *
 * @example
 * expand([10, 13, 16, 21, 30, 45]) // returns  [ [ 3, 3, 5, 9, 15 ], [ 0, 2, 4, 6 ], [ 2, 2, 2 ] ]
 */
let expand: array<int> => sequence = seq => {
  open Array
  seq
  ->unfoldr(xs => {
    xs->all(Fn.eq(0)) ? None : Some(xs, differences(xs))
  })
  ->tail
  ->Sequence
}

/**
 * This function extends a row in a sequence by adding a new element to the end of the row.
 * The new element is the sum of the last element in the row and a given integer `n`.
 * It takes a tuple containing a sequence and an integer, and a row as input.
 * It returns a tuple containing the updated sequence and the new element.
 *
 * @param ((seq, n), row) - A tuple containing a sequence and an integer, and a row.
 *                          `seq` is the sequence to be extended.
 *                          `n` is the integer to be added to the last element in the row.
 *                          `row` is the row to be extended.
 * @returns A tuple containing the updated sequence and the new element.
 *
 * @example
 * extendRow(([ [1, 2, 3], [4, 5, 6] ], 2), [7, 8, 9]) // returns ([ [7, 8, 9, 11], [1, 2, 3], [4, 5, 6] ], 11)
 */
let extendRow: ((array<array<int>>, int), array<int>) => (array<array<int>>, int) = (
  (seq, n),
  row,
) => {
  open Array
  let n' = lastUnsafe(row) + n
  let row' = concat(row, [n'])
  (concat([row'], seq), n')
}

/**
 * This function extends a sequence by adding a new row to the sequence.
 * The new row is created by applying the `extendRow` function to each row in the sequence, starting from the last row.
 * It takes a sequence as input and returns a sequence.
 *
 * @param Sequence(seq) - The input sequence to be extended.
 * @returns A sequence where each row is the result of applying the `extendRow` function to the corresponding row in the input sequence.
 *
 * @example
 * extend(Sequence([ [1, 2, 3], [4, 5, 6] ])) // returns Sequence([ [7, 8, 9, 11], [1, 2, 3], [4, 5, 6] ])
 */
let extend: sequence => sequence = (Sequence(seq)) => {
  open Array
  seq
  ->foldRight(~initial=([], 0), ~f=extendRow)
  ->fst
  ->Sequence
}

/**
 * This function evaluates a sequence by returning the last element of the first row in the sequence.
 * It takes a sequence as input and returns an integer.
 *
 * @param Sequence(seq) - The input sequence to be evaluated.
 * @returns The last element of the first row in the input sequence.
 *
 * @example
 * evaluate(Sequence([ [7, 8, 9, 11], [1, 2, 3], [4, 5, 6] ])) // returns 11
 */
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
  expand([10, 13, 16, 21, 30, 45])->log
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
