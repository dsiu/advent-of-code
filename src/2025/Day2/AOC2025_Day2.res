open Utils
let log = Console.log
let log2 = Console.log2

module Tuple2 = StdlibFp.Tuple2

let bigIntRange = (start: bigint, end_: bigint): array<bigint> => {
  let rec aux = (current: bigint, acc: array<bigint>): array<bigint> =>
    if current > end_ {
      acc->Array.reverse
      acc
    } else {
      aux(current + 1n, Array.concat([current], acc))
    }
  aux(start, [])
}

let parse: string => array<(bigint, bigint)> = data =>
  data
  ->splitNewline
  ->Array.map(String.trim)
  ->Array.join("")
  ->String.split(",")
  ->Array.map(s =>
    s
    ->String.split("-")
    ->Array.map(
      s => {
      s->BigInt.fromString(_)->Option.getExn
      })
    ->Tuple2.fromArray
    ->Option.getExn
  )

let isMirror: string => bool = s => {
  let len = s->String.length
  let half = len / 2
  let firstHalf = s->String.slice(~start=0, ~end=half)
  let secondHalf = s->String.slice(~start=half, ~end=s->String.length)
  firstHalf == secondHalf
}

let isInvalidId: bigint => bool = id => {
  let idStr = id->BigInt.toString
  idStr->String.length->StdlibFp.Int.isEven && idStr->isMirror
}

let solvePart1 = data => {
  let ret = data
  ->parse
  ->Array.map(((start, end)) => {
    let nums = bigIntRange(start, end)
    // nums->Array.filter(isInvalidId)
  //    nums->log2("nums", _)
    [1n,2n]
  })
  ret->Array.map(sumBigIntArray)->sumBigIntArray
}

let solvePart2 = data => {
  data->ignore
  2
}
