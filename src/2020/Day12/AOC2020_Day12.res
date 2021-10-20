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

  let make = {coord: {x: 0, y: 0}, facing: #E}

  module Instruction = {
    exception InvalidStatus(string)

    module Direction = {
      type t = [#North(int) | #East(int) | #South(int) | #West(int)]

      let execute = (ship, s, n) => {
        switch s {
        | "N" => {...ship, coord: {...ship.coord, y: ship.coord.y - n}}
        | "E" => {...ship, coord: {...ship.coord, y: ship.coord.y - n}}
        | "S" => {...ship, coord: {...ship.coord, y: ship.coord.y - n}}
        | "W" => {...ship, coord: {...ship.coord, y: ship.coord.y - n}}
        | _ => raise(InvalidStatus(s))
        }
      }
    }

    module Rotation = {
      type t = [#Left(int) | #Right(int)]

      let execute = (ship, s, n) => {
        switch s {
        | "L" => ship // todo
        | "R" => ship // todo
        | _ => raise(InvalidStatus(s))
        }
      }
    }

    module Move = {
      type t = [#Forward(int)]

      let execute = (ship, s, n) => {
        switch s {
        | "F" => ship // todo
        | _ => raise(InvalidStatus(s))
        }
      }
    }

    type t = Direction(string, int) | Rotation(string, int) | Move(string, int)

    let make = (s, n) => {
      switch s {
      | "N"
      | "E"
      | "S"
      | "W" =>
        Direction(s, n)
      | "L"
      | "R" =>
        Rotation(s, n)
      | "F" => Move(s, n)
      | _ => raise(InvalidStatus(s))
      }
    }
  }

  let execute = (ship, ops) => {
    ops->Array.reduce(ship, (acc, op) => {
      switch op {
      | Instruction.Direction(s, n) => Instruction.Direction.execute(ship, s, n)
      | Instruction.Rotation(s, n) => Instruction.Rotation.execute(ship, s, n)
      | Instruction.Move(s, n) => Instruction.Move.execute(ship, s, n)
      }
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
