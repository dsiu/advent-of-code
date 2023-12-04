@@uncurried

open RescriptCore
open Utils
let log = Console.log

let parse = data => data->splitNewline->Array.map(String.trim)

let get2Digits = str => {
  let digits = str->splitChars->Array.map(Int.fromString(~radix=10))->Array.keepSome
  let first = digits->Array.at(0)->Option.getUnsafe
  let second = digits->Array.at(-1)->Option.getUnsafe
  first * 10 + second
}

let part1 = xs => {
  let result = xs->Array.map(get2Digits)
  result->sumIntArray
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->ignore
  2
}
