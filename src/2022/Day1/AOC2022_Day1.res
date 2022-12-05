open Belt
open Utils
let log = Js.Console.log

let parse = data =>
  data->splitDoubleNewline->Array.map(x => x->splitNewline->Array.map(intFromStringExn))

let part1 = data => {
  data->Array.map(sumIntArray)->maxIntInArray
}

let part2 = data => {
  let sorted = data->Array.map(sumIntArray)->SortArray.Int.stableSort
  sorted->Array.sliceToEnd(sorted->Array.length - 3)->sumIntArray
}

let solvePart1 = data => {
  data->parse->part1
}

let solvePart2 = data => {
  data->parse->part2
}
