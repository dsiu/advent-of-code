@@uncurried

open RescriptCore
open Utils
let log = Js.Console.log

let parse = data => data->splitNewline->Array.map(String.trim)

let solvePart1 = data => {
  data->log
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
