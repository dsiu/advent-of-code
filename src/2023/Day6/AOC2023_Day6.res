@@uncurried

open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

type race = {
  time: int,
  distance: int,
}

let raceToString: race => string = ({time, distance}) => {
  `(time: ${time->Int.toString}, distance: ${distance->Int.toString})`
}

let raceListToString = Utils.Printable.List.toString(_, raceToString)

module SheetParser = {
  module P = ReludeParse.Parser
  open P.Infix

  let justSpaceP: P.t<unit> = P.void(P.many(P.str(" ")))
  let debug = P.tapLog

  let numbersP = P.sepBy(justSpaceP, P.anyInt)

  let timesP = P.str("Time:")->\"*>"(justSpaceP)->\"*>"(numbersP)

  let distancesP = justSpaceP->\"*>"(P.str("Distance:"))->\"*>"(justSpaceP)->\"*>"(numbersP)

  let mkRace = (a, b) => {
    List.map2(a, b, ~f=(time, distance) => {time, distance})
  }

  let racesP = mkRace->\"<$>"(timesP->\"<*"(P.eol))->\"<*>"(distancesP)

  let run: string => list<race> = str => {
    P.runParser(str, racesP)->Result.getExn
  }
}

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

  Array.map2(times, distances, ~f=(time, distance) => {time, distance})
}

let solvePart1 = data => {
  SheetParser.run(data)->raceListToString->log
  //  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
