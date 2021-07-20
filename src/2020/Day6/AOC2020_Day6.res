open Belt
let log = Js.Console.log
//open Utils

// probably move these split functions to util?
let splitChars = Js.String2.split(_, "")
let splitNewline = Js.String2.split(_, "\n")
let splitDoubleNewline = Js.String2.split(_, "\n\n")
let sum = (a, x) => a + x
let sumIntArray = Array.reduce(_, 0, sum)
let join = Js.Array2.joinWith(_, "")

let parsePart1 = x => x->splitNewline->Array.map(Js.String2.trim)->join->splitChars

let countUnique = x => x->Set.String.fromArray->Set.String.size

let parse = (data, f) => data->splitDoubleNewline->Array.map(f)

let filled = {
  "abcdefghijklmnopqrstuvwxyz"->splitChars->Set.String.fromArray
}

let parsePart2 = x =>
  x
  ->splitNewline
  ->Array.map(Js.String2.trim)
  ->Array.map(splitChars)
  ->Array.map(Set.String.fromArray)
  ->Array.reduce(_, filled, Set.String.intersect)
  ->Set.String.size

let solvePart1 = data => {
  let part1Parser = parse(_, parsePart1)
  data->part1Parser->Array.map(countUnique)->sumIntArray
}

let solvePart2 = data => {
  let part2Parser = parse(_, parsePart2)
  data->part2Parser->sumIntArray
}
