open Belt
open Utils
let log = Js.Console.log

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
