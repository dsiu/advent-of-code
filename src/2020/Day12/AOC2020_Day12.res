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
    wayPoint: point, // relative to ship coord
  }

  let make = {coord: {x: 0, y: 0}, facing: #E, wayPoint: {x: 10, y: 1}}

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

  let rotateShipLeft = (t, degree) => {
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

  let rotateShipRight = (t, degree) => {
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

  let move = (point, facing, n) => {
    switch facing {
    | #N => {...point, y: point.y + n}
    | #E => {...point, x: point.x + n}
    | #S => {...point, y: point.y - n}
    | #W => {...point, x: point.x - n}
    }
  }

  let moveShip = (t, direction, n) => {
    {...t, coord: t.coord->move(direction, n)}
  }

  let moveWayPoint = (t, direction, n) => {
    {...t, wayPoint: t.wayPoint->move(direction, n)}
  }

  let moveShipTowardWayPoint = (t, n) => {
    {...t, coord: {x: t.coord.x + t.wayPoint.x * n, y: t.coord.y + t.wayPoint.y * n}}
  }

  let l90WayPoint = t => {
    {...t, wayPoint: {x: -t.wayPoint.y, y: t.wayPoint.x}}
  }

  let r90WayPoint = t => {
    {...t, wayPoint: {x: t.wayPoint.y, y: -t.wayPoint.x}}
  }

  let flip180WayPoint = t => {
    {...t, wayPoint: {x: -t.wayPoint.x, y: -t.wayPoint.y}}
  }

  let rotateWayPointLeft = (t, degree) => {
    switch degree {
    | 90
    | -270 =>
      t->l90WayPoint
    | -90
    | 270 =>
      t->r90WayPoint
    | 180
    | -180 =>
      t->flip180WayPoint
    | 360 => t
    | _ => InvalidStatus(degree->Int.toString)->raise
    }
  }

  let rotateWayPointRight = (t, degree) => {
    switch degree {
    | 90
    | -270 =>
      t->r90WayPoint
    | -90
    | 270 =>
      t->l90WayPoint
    | 180
    | -180 =>
      t->flip180WayPoint
    | 360 => t
    | _ => InvalidStatus(degree->Int.toString)->raise
    }
  }

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
      | #North(n) => ship->moveShip(#N, n)
      | #East(n) => ship->moveShip(#E, n)
      | #South(n) => ship->moveShip(#S, n)
      | #West(n) => ship->moveShip(#W, n)
      | #Left(n) => ship->rotateShipLeft(n)
      | #Right(n) => ship->rotateShipRight(n)
      | #Forward(n) => ship->moveShip(ship.facing, n)
      }
    }

    let executeWithWayPoint = (ship, s) => {
      switch s {
      | #North(n) => ship->moveWayPoint(#N, n)
      | #East(n) => ship->moveWayPoint(#E, n)
      | #South(n) => ship->moveWayPoint(#S, n)
      | #West(n) => ship->moveWayPoint(#W, n)
      | #Left(n) => ship->rotateWayPointLeft(n)
      | #Right(n) => ship->rotateWayPointRight(n)
      | #Forward(n) => ship->moveShipTowardWayPoint(n)
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

  let execute = (ship, ops, algo) => {
    ops->Array.reduce(ship, (acc, op) => {
      algo(acc, op)
    })
  }

  let part1Algo = Instruction.execute
  let part2Algo = Instruction.executeWithWayPoint
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(x => {
    let s = x->Js.String2.trim
    let code = s->Js.String2.charAt(0)
    let n = s->Js.String2.substringToEnd(~from=1)->intFromStringExn
    (code, n)
  })

let solve = (data, algo) => {
  let ops = data->parse->Array.map(((code, n)) => {Ship.Instruction.make(code, n)})
  let ship = Ship.make
  let done = ship->Ship.execute(ops, algo)
  //  done->log
  [done.coord.x, done.coord.y]->Array.map(Js.Math.abs_int)->Array.reduce(0, add)
}

let solvePart1 = solve(_, Ship.part1Algo)

let solvePart2 = solve(_, Ship.part2Algo)
