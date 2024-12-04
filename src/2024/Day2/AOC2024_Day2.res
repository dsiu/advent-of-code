open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

let isCondMet = condition => Array.every(_, condition)

let isInc = isCondMet(x => x > 0)
let isDec = isCondMet(x => x < 0)
let isDiffMinOne = isCondMet(x => Math.Int.abs(x) >= 1)
let isDiffMaxThree = isCondMet(x => Math.Int.abs(x) <= 3)

let diff = (x, y) => y - x

/**
 * Checks if an array is "safe" based on specific conditions.
 * @param {array<int>} xs - The input array of integers.
 * @returns {option<array<int>>} - Returns Some(xs) if the array is safe, otherwise None.
 */
let isSafe = xs => {
  let a = xs->Array.slice(~start=0, ~end=xs->Array.length - 1)
  let b = xs->Array.tail

  // Calculate differences between consecutive elements
  let diffs = Array.zipWith(a, b, diff)

  // Check if differences meet conditions
  let safe = (diffs->isInc || diffs->isDec) && diffs->isDiffMinOne && diffs->isDiffMaxThree

  safe ? Some(xs) : None
}

/**
 * Removes the nth element from an array.
 * @param {array<'a>} xs - The input array.
 * @param {int} i - The index of the element to remove.
 * @returns {array<'a>} - A new array with the nth element removed.
 */
let removeNthElem = (xs, i) => Array.toSpliced(xs, ~start=i, ~remove=1, ~insert=[])

let isSafeWithTolerance = xs => {
  xs
  ->isSafe
  ->Option.orElse({
    let subReports = xs->Array.reduceWithIndex([], (acc, _, i) => {
      acc->Array.push(xs->removeNthElem(i))
      acc
    })

    subReports->Array.find(r => r->isSafe->Option.isSome)
  })
}

let countCondMet = (xs, cond) => xs->Array.filter(x => cond(x)->Option.isSome)->Array.length

let part1 = reports => {
  reports->countCondMet(isSafe)
}

let part2 = reports => {
  reports->countCondMet(isSafeWithTolerance)
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => x->String.trim->String.split(" ")->Array.filterMap(Int.fromString(_)))

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
