open Belt
let log = Js.Console.log
open Utils

// probably move these split functions to util?

let parsePart1 = x => x->splitNewline->Array.map(Js.String2.trim)->join->splitChars

let countUnique = x => x->Set.String.fromArray->Set.String.size

let parse = (data, f) => data->splitDoubleNewline->Array.map(f)

let filled = {
  "abcdefghijklmnopqrstuvwxyz"->splitChars->Set.String.fromArray
}

let parsePart2 = x => {
  let {map, reduce} = module(Belt.Array)
  x
  ->splitNewline
  ->map(Js.String2.trim)
  ->map(splitChars)
  ->map(Set.String.fromArray)
  ->reduce(_, filled, Set.String.intersect)
  ->Set.String.size
}

let solvePart1 = data => {
  let part1Parser = parse(_, parsePart1)
  data->part1Parser->Array.map(countUnique)->sumIntArray
}

let solvePart2 = data => {
  let part2Parser = parse(_, parsePart2)
  data->part2Parser->sumIntArray
}
