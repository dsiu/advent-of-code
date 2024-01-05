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

let waysToWin: race => int = ({time, distance}) => {
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

let part1: array<race> => int = races => {
  races->Array.map(waysToWin)->Array.fold(~initial=1, ~f=Int.multiply)
}

let part2: array<race> => int = races => {
  // convert list of times and distances into string then concat them
  let r' = races->Array.fold(~initial=("", ""), ~f=((aTime, aDistance), {time, distance}) => {
    (aTime ++ time->BigInt.toString, aDistance ++ distance->BigInt.toString)
  })

  let (t', d') = r'

  // there is only 1 race to test
  waysToWin({time: t'->BigInt.fromString, distance: d'->BigInt.fromString})
}

// using Monadic Parser Combinators
module SheetParser = {
  module P = ReludeParse.Parser
  open P.Infix

  let justSpaceP: P.t<unit> = P.void(P.many(P.str(" ")))
  let debug = P.tapLog

  let numbersP = P.sepBy(justSpaceP, P.anyInt)

  let timesP = P.str("Time:")->\"*>"(justSpaceP)->\"*>"(numbersP)

  let distancesP = justSpaceP->\"*>"(P.str("Distance:"))->\"*>"(justSpaceP)->\"*>"(numbersP)

  let mkRace = (a, b) => {
    List.map2(a, b, ~f=(time, distance) => mkRaceFromInt(time, distance))
  }

  let racesP = mkRace->\"<$>"(timesP->\"<*"(P.eol))->\"<*>"(distancesP)

  let run: string => array<race> = str => {
    P.runParser(str, racesP)->Result.getExn->List.toArray
  }
}

// manually written parser
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

  Array.map2(times, distances, ~f=(time, distance) => mkRaceFromInt(time, distance))
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
