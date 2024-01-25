@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type race = {
  time: BigInt.t,
  distance: BigInt.t,
}

let mkRaceFromInt = (time, distance) => {
  {time: time->BigInt.fromInt, distance: distance->BigInt.fromInt}
}

let raceToString: race => string = ({time, distance}) => {
  `(time: ${time->BigInt.toString}, distance: ${distance->BigInt.toString})`
}

let raceListToString = Utils.Printable.List.toString(_, raceToString)
let raceArrayToString = Utils.Printable.Array.toString(_, raceToString)

// PATCH
//
// copy from RescriptCore.Array and make it work with BigInt
//
module BigArray = {
  open BigInt
  @new external makeUninitializedUnsafe: BigInt.t => array<'a> = "Array"
  external setUnsafe: (array<'a>, BigInt.t, 'a) => unit = "%array_unsafe_set"

  let fromInitializer = (~length: BigInt.t, f) =>
    if length <= 0->fromInt {
      []
    } else {
      let arr = makeUninitializedUnsafe(length)
      let init = 0->fromInt
      let end = length - 1->fromInt

      let i = ref(init)
      while i.contents <= end {
        arr->setUnsafe(i.contents, f(i.contents))
        i := i.contents + 1->fromInt
      }
      arr
    }
}

/**
 * Function to calculate the number of ways to win a race.
 *
 * @param {Object} race - An object representing a race.
 * @param {BigInt} race.time - The time taken for the race.
 * @param {BigInt} race.distance - The distance of the race.
 *
 * @returns {int} The number of ways to win the race.
 *
 * The function works by creating an array `h` of BigInts from 1 to `time - 1`.
 * Then it filters `h` to only include those elements `h` for which `(time - h) * h` is greater than `distance`.
 * The length of the resulting array is the number of ways to win the race.
 */
let waysToWinBurteForce: race => int = ({time, distance}) => {
  open BigInt
  let h = BigArray.fromInitializer(~length=time - 1->fromInt, i => i + 1->fromInt)

  h
  ->Array.filterMap(h => {
    switch (time - h) * h {
    | d if d > distance => Some(d)
    | _ => None
    }
  })
  ->Array.length
}

//@val external isInteger: float => bool = "Number.isInteger"

let waysToWin: race => int = ({time, distance}) => {
  // winning case: x(T-x) > D where T = time, D = distance, x = button's press time
  //
  // x^2 -Tx + D > 0
  //
  // quadractic formula solution
  // a = (T-sqar(T^2 -4D))/2
  // b = (T+sqar(T^2 -4D))/2
  let t = time->BigInt.toFloat
  let d = distance->BigInt.toFloat

  open Float
  let a = (t -. Math.sqrt(t->Math.pow(~exp=2.) -. 4. *. d)) /. 2.
  let b = (t +. Math.sqrt(t->Math.pow(~exp=2.) -. 4. *. d)) /. 2.

  let a = a->isInteger ? a +. 1. : Math.ceil(a)
  let b = b->isInteger ? b -. 1. : Math.floor(b)

  //  let a = Math.ceil(a)
  //  let b = Math.floor(b)

  // number of winning cases is b-a+1

  (b -. a +. 1.)->Int.fromFloat
}

/**
 * Function to calculate the total number of ways to win for an array of races.
 *
 * @param {array<race>} races - An array of races.
 *
 * @returns {int} The total number of ways to win all the races.
 *
 * The function works by mapping over the array of races and applying the `waysToWin` function to each race.
 * Then it folds over the resulting array of ways to win each race, multiplying all the results together.
 * The result is the total number of ways to win all the races.
 */
let part1: array<race> => int = races => {
  races->Array.map(waysToWin)->Array.fold(~initial=1, ~f=Int.multiply)
}

/**
 * Function to calculate the total number of ways to win for a single race derived from an array of races.
 *
 * @param {array<race>} races - An array of races.
 *
 * @returns {int} The total number of ways to win the derived race.
 *
 * The function works by folding over the array of races and concatenating the time and distance of each race into two separate strings.
 * Then it calls the `waysToWin` function with these concatenated strings converted back to BigInts.
 * The result is the total number of ways to win the derived race.
 */
let part2: array<race> => int = races => {
  // convert list of times and distances into string then concat them
  let r' = races->Array.fold(~initial=("", ""), ~f=((aTime, aDistance), {time, distance}) => {
    (aTime ++ time->BigInt.toString, aDistance ++ distance->BigInt.toString)
  })

  let (t', d') = r'

  // there is only 1 race to test
  waysToWin({time: t'->BigInt.fromString, distance: d'->BigInt.fromString})
}

// This module provides functionality to parse a string representing a race into an array of `race` objects.
// It uses the `ReludeParse.Parser` library for parsing.
module SheetParser = {
  module P = ReludeParse.Parser
  open P.Infix

  // A parser that matches any number of spaces in the input string and discards the result.
  let justSpaceP: P.t<unit> = P.void(P.many(P.str(" ")))

  // A utility function for logging the current state of the parser. Useful for debugging.
  let debug = P.tapLog

  // A parser that matches a sequence of integers separated by spaces in the input string.
  let numbersP = P.sepBy(justSpaceP, P.anyInt)

  // A parser that matches the string "Time:" followed by a sequence of integers (representing times) separated by spaces.
  let timesP = P.str("Time:")->\"*>"(justSpaceP)->\"*>"(numbersP)

  // A parser that matches the string "Distance:" followed by a sequence of integers (representing distances) separated by spaces.
  let distancesP = justSpaceP->\"*>"(P.str("Distance:"))->\"*>"(justSpaceP)->\"*>"(numbersP)

  // A function that takes two lists of integers (representing times and distances) and returns a list of `race` objects.
  // Each `race` object is created by calling the `mkRaceFromInt` function with a pair of corresponding time and distance.
  let mkRace = (a, b) => {
    List.map2(a, b, (time, distance) => mkRaceFromInt(time, distance))
  }

  // A parser that matches a sequence of races in the input string.
  // Each race is represented by a "Time:" line followed by a "Distance:" line.
  let racesP = mkRace->\"<$>"(timesP->\"<*"(P.eol))->\"<*>"(distancesP)

  // A function that takes a string, runs the `racesP` parser on it, and returns an array of `race` objects.
  // If the parser fails, an exception is thrown.
  let run: string => array<race> = str => {
    P.runParser(str, racesP)->Result.getExn->List.toArray
  }
}

// manually written parser (not using it)
let parse = data => {
  let lines =
    data
    ->splitNewline
    ->Array.map(String.trim)

  let parseLine = (line, prefix) => {
    line
    ->Option.map(s =>
      s
      ->String.replace(prefix, "")
      ->splitSpace
      ->Array.filter(Fn.complement(String.isEmpty))
      ->Array.map(Fn.compose(Int.fromString, Option.getUnsafe))
    )
    ->Option.getExn
  }

  let times = parseLine(lines[0], "Time:")

  let distances = parseLine(lines[1], "Distance:")

  Array.map2(times, distances, (time, distance) => mkRaceFromInt(time, distance))
}

let solvePart1 = data => {
  // SheetParser.run(data)->raceArrayToString->log
  data->SheetParser.run->part1
}

let solvePart2 = data => {
  Console.warn("MUST use bigger heap to run.  try `node index.js --max-old-space-size=8192` (8GB of heap) or `export NODE_OPTIONS=--max-old-space-size=8192
")
  data->SheetParser.run->part2
}
