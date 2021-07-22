open Belt
let log = Js.Console.log
open Utils

module Rules = {
  type t = {
    count: int,
    bag: string,
  }
}

let parseLine = l =>
  l->Js.String2.trim->Js.String2.splitAtMost(_, "contain", ~limit=2)->Array.map(Js.String2.trim)

let parse = data => data->splitNewline->Array.map(parseLine)

let solvePart1 = data => {
  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
