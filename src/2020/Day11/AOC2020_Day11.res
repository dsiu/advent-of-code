open Belt
let log = Js.Console.log
open Utils

module SeatMap = {
  type t

  module SeatStatus = {
    type t = Occupied | Empty | Floor
  }

  let make = (xs: array<string>) => {
    let x = xs->Array.getExn(0)->Js.String2.length
    let y = xs->Array.length
    let ret = Array2D.make((x, y), SeatStatus.Floor)
  }
}

let parse = data => {
  let parsed = data->Js.String2.split("\n")->Array.map(Js.String2.trim)
  SeatMap.make(parsed)
}

let solvePart1 = data => {
  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
