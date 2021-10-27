open Belt
open Utils
let log = Js.Console.log

exception InvalidStatus(string)

module Ship = {
  type facing = [#N | #E | #S | #W]
  type point = {x: int, y: int}
  type t = {
    coord: point,
    facing: facing,
    wayPoint: point,
  }

  let flip180 = facing => {
    switch facing {
    | #N => #S
    | #E => #W
    | #S => #N
    | #W => #E
    }
  }

  let l90 = facing => {
    switch facing {
    | #N => #W
    | #E => #N
    | #S => #E
    | #W => #S
    }
  }

  let r90 = facing => {
    switch facing {
    | #N => #E
    | #E => #S
    | #S => #W
    | #W => #N
    }
  }

  let rotateLeft = (t, degree) => {
    switch (t.facing, degree) {
    | (d, 90)
    | (d, -270) => {...t, facing: d->l90}
    | (d, -90)
    | (d, 270) => {...t, facing: d->r90}
    | (d, 180)
    | (d, -180) => {...t, facing: d->flip180}
    | (_, 360) => t
    | (_, _) => InvalidStatus(degree->Int.toString)->raise
    }
  }

  let rotateRight = (t, degree) => {
    switch (t.facing, degree) {
    | (d, 90)
    | (d, -270) => {...t, facing: d->r90}
    | (d, -90)
    | (d, 270) => {...t, facing: d->l90}
    | (d, 180)
    | (d, -180) => {...t, facing: d->flip180}
    | (_, 360) => t
    | (_, _) => InvalidStatus(degree->Int.toString)->raise
    }
  }

  let m = (point, facing, n) => {
    switch facing {
    | #N => {...point, y: point.y - n}
    | #E => {...point, x: point.x + n}
    | #S => {...point, y: point.y + n}
    | #W => {...point, x: point.x - n}
    }
  }

  let move = (t, direction, n) => {
    {...t, coord: t.coord->m(direction, n)}
  }

  let moveWP = (t, direction, n) => {
    {...t, wayPoint: t.wayPoint->m(direction, n)}
  }

  let relDirection = (a, b) => {
    let xDir = switch b.x - a.x {
    | 0 => None
    | x =>
      if x > 0 {
        Some(#E)
      } else {
        Some(#E)
      }
    }

    let yDir = switch b.y - a.y {
    | 0 => None
    | y =>
      if y > 0 {
        Some(#N)
      } else {
        Some(#S)
      }
    }

    [xDir, yDir]
  }

    let forwardWP = (t, n) => {
      let moves = relDirection(t.coord, t.wayPoint)
      moves->Array.reduce(t, (a, m) => {
        a->move(m->Option.getExn, n*)
      })
    }

  let make = {coord: {x: 0, y: 0}, facing: #E, wayPoint: {x: 10, y: 1}}

  module Instruction = {
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
      | #North(n) => ship->move(#N, n)
      | #East(n) => ship->move(#E, n)
      | #South(n) => ship->move(#S, n)
      | #West(n) => ship->move(#W, n)
      | #Left(n) => ship->rotateLeft(n)
      | #Right(n) => ship->rotateRight(n)
      | #Forward(n) => ship->move(ship.facing, n)
      }
    }

    let executeWithWayPoint = (ship, s) => {
      switch s {
      | #North(n) => ship->moveWP(#N, n)
      | #East(n) => ship->moveWP(#E, n)
      | #South(n) => ship->moveWP(#S, n)
      | #West(n) => ship->moveWP(#W, n)
      | #Left(n) => ship->rotateLeft(n)
      | #Right(n) => ship->rotateRight(n)
      | #Forward(n) => ship->forwardWP(ship, n)
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

  let executeWithWayPoint = (ship, ops) => {
    ops->Array.reduce(ship, (acc, op) => {
      Instruction.executeWithWayPoint(acc, op)
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
  let done = ship->Ship.execute(ops)
  done->log
  [done.coord.x, done.coord.y]->Array.map(Js.Math.abs_int)->Array.reduce(0, sum)
}

let solvePart2 = data => {
  let ops = data->parse->Array.map(((code, n)) => {Ship.Instruction.make(code, n)})
  let ship = Ship.make
  let done = ship->Ship.executeWithWayPoint(ops)
  done->log
  [done.coord.x, done.coord.y]->Array.map(Js.Math.abs_int)->Array.reduce(0, sum)
}
