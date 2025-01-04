open Stdlib
open Utils
let log = Console.log
let log2 = Console.log2

let parse = data => data->splitNewline->Array.map(String.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}