open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

let parse = data => data->String.trim

let calMultiplication = s => {
  let mulPattern = /mul\(\d+,\d+\)/g
  let argsPattern = /(\d+),(\d+)/

  let muls = s->String.match(mulPattern)->Option.getUnsafe

  let result = muls->Array.map(args => {
    RegExp.exec(argsPattern, args->Option.getExn)->Option.mapOr([0], r =>
      r
      ->RegExp.Result.matches
      ->Array.map(a => a->(Int.fromString(_))->Option.getExn)
    )
  })
  result->Array.map(mulIntArray)
}

let part1 = s => calMultiplication(s)->sumIntArray

let part2 = s => {
  s
  ->String.split("do()")
  ->Array.map(x => x->String.split("don't()")->Array.getUnsafe(0))
  ->Array.map(calMultiplication)
  ->Array.map(sumIntArray)
  ->sumIntArray
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
