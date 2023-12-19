@@uncurried

open RescriptCore
open Utils
let log = Console.log
let log2 = Console.log2

module IntervalMap = {
  type t = {
    srcInterval: Interval.t,
    destInterval: Interval.t,
  }

  let toString: t => string = t => {
    `SrcDestInterval(Src:${t.srcInterval->Interval.toString}, Dest:${t.destInterval->Interval.toString})`
  }

  let srcToDest: (t, BigInt.t) => option<BigInt.t> = (t, srcNum) => {
    open! BigInt
    t.srcInterval->Interval.inInterval(srcNum)
      ? {
          let offset = srcNum - t.srcInterval.lower

          Some(t.destInterval.lower + offset)
        }
      : None
  }
}

module AlmanacMap = {
  type t = {
    srcCategory: string,
    destCategory: string,
    intervals: array<IntervalMap.t>,
  }

  let toString: t => string = t => {
    `AlmanacMap(${t.srcCategory}, ${t.destCategory}, ${t.intervals
      ->Array.map(IntervalMap.toString)
      ->Array.joinWith(", ")})`
  }

  let srcToDest: (t, BigInt.t) => BigInt.t = (t, srcNum) => {
    t.intervals
    ->Array.findMap(r => r->IntervalMap.srcToDest(srcNum))
    ->Option.getWithDefault(srcNum)
  }
}

module Almanac = {
  type t = {
    seeds: array<BigInt.t>,
    maps: array<AlmanacMap.t>,
  }

  let toString: t => string = t => {
    `Almanac (Seeds: ${t.seeds->Array.map(BigInt.toString)->Array.joinWith(", ")}\n${t.maps
      ->Array.map(AlmanacMap.toString)
      ->Array.joinWith("\n")})`
  }

  let getMap: (t, string) => AlmanacMap.t = (t, src) => {
    t.maps->Array.find(m => m.srcCategory == src)->Option.getExn
  }
}

let parse: string => Almanac.t = data => {
  let lines = data->splitDoubleNewline->Array.map(l => l->splitNewline->Array.map(String.trim))
  let seedLine = lines[0]->Option.flatMap(Array.get(_, 0))->Option.getExn
  let mapLines = lines->(Array.sliceToEnd(_, ~start=1))

  let parseSeed: string => array<BigInt.t> = line => {
    line->String.split(": ")->Array.get(1)->Option.getExn->splitSpace->Array.map(BigInt.fromString)
  }

  let parseMap: array<string> => AlmanacMap.t = lines => {
    let categoryLine = lines[0]->Option.getExn
    let srcDestLines = lines->Array.sliceToEnd(~start=1)

    let [srcCategory, destCategory] =
      categoryLine
      ->splitSpace
      ->Array.get(0)
      ->Option.flatMap(s => s->String.split(_, "-to-")->Some)
      ->Option.getExn

    let parseIntervalLine: string => IntervalMap.t = l => {
      let [destStart, srcStart, len] = l->splitSpace->Array.map(BigInt.fromString)
      let one = BigInt.fromInt(1)
      open! BigInt
      {
        srcInterval: {lower: srcStart, upper: srcStart + len - one},
        destInterval: {lower: destStart, upper: destStart + len - one},
      }
    }

    let intervals = srcDestLines->Array.map(parseIntervalLine)
    {srcCategory, destCategory, intervals}
  }

  let seeds = parseSeed(seedLine)

  let maps = mapLines->Array.map(parseMap)
  //  {seeds, maps}
  {seeds, maps}
}

let part1 = ({seeds, _} as almanac: Almanac.t) => {
  let startCat = "seed"
  let endCat = "location"

  let locations = seeds->Array.map(s => {
    let rec loop = (endCat, curCat, curNum) => {
      let map = almanac->Almanac.getMap(curCat)
      let {destCategory} = map
      let nextNum = AlmanacMap.srcToDest(map, curNum)
      destCategory == endCat ? nextNum : loop(endCat, destCategory, nextNum)
    }

    loop(endCat, startCat, s)
  })

  locations->log
  locations->Utils.minBigIntInArray
}

let solvePart1 = data => {
  let almanac = data->parse
  almanac->Almanac.toString->log
  let m = almanac->Almanac.getMap("seed")
  AlmanacMap.srcToDest(m, BigInt.fromInt(79))->BigInt.toString->log
  AlmanacMap.srcToDest(m, BigInt.fromInt(14))->BigInt.toString->log
  AlmanacMap.srcToDest(m, BigInt.fromInt(55))->BigInt.toString->log
  AlmanacMap.srcToDest(m, BigInt.fromInt(13))->BigInt.toString->log

  part1(almanac)
}

let solvePart2 = data => {
  data->ignore
  2
}
