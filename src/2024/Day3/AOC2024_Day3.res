open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

let parse = data => data->String.trim

let exec = s => {
  let mulExp = /mul\(\d+,\d+\)/g
  let mulArgsExp = /(\d+),(\d+)/

  let muls = s->String.match(mulExp)->Option.getUnsafe

  let result = muls->Array.map(x => {
    let args = RegExp.exec(mulArgsExp, x->Option.getExn)
    switch args {
    | Some(r) =>
      r
      ->RegExp.Result.matches
      ->Array.map(a => {
        a->Int.fromString->Option.getExn
      })
    | None => [0]
    }
  })

  result->Array.map(mulIntArray)
}

let part1 = s => exec(s)->sumIntArray

let part2 = s => {
  s
  ->String.split("do()")
  ->Array.map(x => x->String.split("don't()")->Array.getUnsafe(0))
  ->Array.map(exec)
  ->Array.map(sumIntArray)
  ->sumIntArray
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
