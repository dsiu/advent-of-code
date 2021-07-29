open Belt
let log = Js.Console.log
open Utils

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim
  })

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
