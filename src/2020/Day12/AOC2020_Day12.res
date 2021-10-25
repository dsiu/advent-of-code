open Belt
open Utils
let log = Js.Console.log

module Ship = {
  type facing = [#N | #E | #S | #W]
  type point = {x: int, y: int}
  type t = {
    coord: point,
    facing: facing,
  }

  let rotateLeft = (t, degree) => {}
  let rotateRight = (t, degree) => {}

  let make = {coord: {x: 0, y: 0}, facing: #E}

  module Instruction = {
    exception InvalidStatus(string)

    type t = [
      | #North(int)
      | #East(int)
      | #South(int)
      | #West(int)
      | #Left(int)
      | #Right(int)
      | #Forward(int)
    ]

    let execute = (ship, s) => {
      switch s {
      | #North(n) => {...ship, coord: {...ship.coord, y: ship.coord.y - n}}
      | #East(n) => {...ship, coord: {...ship.coord, x: ship.coord.x + n}}
      | #South(n) => {...ship, coord: {...ship.coord, y: ship.coord.y + n}}
      | #West(n) => {...ship, coord: {...ship.coord, x: ship.coord.x - n}}
      | #Left(n) => ship // todo
      | #Right(n) => ship // todo
      | #Forward(n) => ship // todo
      }
    }

    let make = (s, n) => {
      switch s {
      | "N" => #North(n)
      | "E" => #East(n)
      | "S" => #South(n)
      | "W" => #West(n)
      | "L" => #Left(n)
      | "R" => #Right(n)
      | "F" => #Forward(n)
      | _ => raise(InvalidStatus(s))
      }
    }
  }

  let execute = (ship, ops) => {
    ops->Array.reduce(ship, (acc, op) => {
      Instruction.execute(acc, op)
    })
  }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    let s = x->Js.String2.trim
    let code = s->Js.String2.charAt(0)
    let n = s->Js.String2.substringToEnd(~from=1)->Int.fromString->Option.getExn
    (code, n)
  })

let solvePart1 = data => {
  let ops = data->parse->Array.map(((code, n)) => {Ship.Instruction.make(code, n)})
  let ship = Ship.make
  ship->Ship.execute(ops)->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
