open Belt
let log = Js.Console.log
open Utils

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->parse
}

let solvePart2 = data => {
  data->ignore
  2
}
