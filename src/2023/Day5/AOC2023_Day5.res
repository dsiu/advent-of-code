@@uncurried

open RescriptCore
open Utils
let log = Console.log
let log2 = Console.log2

module Range = {
  type t = {
    start: BigInt.t,
    end: BigInt.t,
  }

  let toString: t => string = t => {
    `Range(${t.start->BigInt.toString}, ${t.end->BigInt.toString})`
  }

  let inRange: (t, BigInt.t) => bool = (t, num) => {
    num >= t.start && num <= t.end
  }
}

module SrcDestRange = {
  type t = {
    srcRange: Range.t,
    destRange: Range.t,
  }

  let toString: t => string = t => {
    `SrcDestRange(Src:${t.srcRange->Range.toString}, Dest:${t.destRange->Range.toString})`
  }

  let srcToDest: (t, BigInt.t) => option<BigInt.t> = (t, srcNum) => {
    open! BigInt
    t.srcRange->Range.inRange(srcNum)
      ? {
          let offset = srcNum - t.srcRange.start

          Some(t.destRange.start + offset)
        }
      : None
  }
}

module AlmanacMap = {
  type t = {
    srcCategory: string,
    destCategory: string,
    ranges: array<SrcDestRange.t>,
  }

  let toString: t => string = t => {
    `AlmanacMap(${t.srcCategory}, ${t.destCategory}, ${t.ranges
      ->Array.map(SrcDestRange.toString)
      ->Array.joinWith(", ")})`
  }

  let srcToDest: (t, BigInt.t) => BigInt.t = (t, srcNum) => {
    t.ranges->Array.findMap(r => r->SrcDestRange.srcToDest(srcNum))->Option.getWithDefault(srcNum)
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

    let parseRangeLine: string => SrcDestRange.t = l => {
      let [destStart, srcStart, len] = l->splitSpace->Array.map(BigInt.fromString)
      let one = BigInt.fromInt(1)
      open! BigInt
      {
        srcRange: {start: srcStart, end: srcStart + len - one},
        destRange: {start: destStart, end: destStart + len - one},
      }
    }

    let ranges = srcDestLines->Array.map(parseRangeLine)
    {srcCategory, destCategory, ranges}
  }

  let seeds = parseSeed(seedLine)

  let maps = mapLines->Array.map(parseMap)
  //  {seeds, maps}
  {seeds, maps}
}

let solvePart1 = data => {
  let almanac = data->parse
  almanac->Almanac.toString->log
  let m = almanac->Almanac.getMap("seed")
  AlmanacMap.srcToDest(m, BigInt.fromInt(79))->BigInt.toString->log
  AlmanacMap.srcToDest(m, BigInt.fromInt(14))->BigInt.toString->log
  AlmanacMap.srcToDest(m, BigInt.fromInt(55))->BigInt.toString->log
  AlmanacMap.srcToDest(m, BigInt.fromInt(13))->BigInt.toString->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
