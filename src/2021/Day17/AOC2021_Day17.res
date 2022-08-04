open Belt
//open Utils
let log = Js.Console.log

module TrickShot = {
  type velocity = {x: int, y: int}
  type coord = (int, int)

  let velocityNext = ({x, y}: velocity) => {
    x: x == 0 ? 0 : x < 0 ? x + 1 : x - 1,
    y: y - 1,
  }

  type target = {x_min: int, x_max: int, y_min: int, y_max: int}

  let isTargetHit_ = ((x, y): coord, {x_min, x_max, y_min, y_max}: target) => {
    x >= x_min && x <= x_max && y >= y_min && y <= y_max
  }

  let isTargetHit = t_init => isTargetHit_(_, t_init)

  let isOutOfRange_ = ((x, y): coord, {x_min, x_max, y_min, y_max}: target) => {
    //    j`isOutOfRange $x, $y, {$x_min, $x_max, $y_min, $y_max}`->log
    x > x_max || y < y_min
  }

  let isOUtOfRange = t_init => isOutOfRange_(_, t_init)

  type trajectory = array<coord>

  type launchResult =
    | Hit(coord, trajectory)
    | Miss(trajectory)

  let dump = r => {
    let trajectory_str = Array.map(_, ((x, y)) => j`($x, $y)\n`)
    switch r {
    | Hit((x, y), t) => {
        let t_str = t->trajectory_str
        j`Hit: ($x, $y) | trajectory: [$t_str]`
      }
    | Miss(t) => {
        let t_str = t->trajectory_str
        j`Miss: trajectory: [$t_str]`
      }
    }
  }

  let launch = (v0, target) => {
    let isHit = isTargetHit(target)
    let isOutOfRange = isOUtOfRange(target)

    let rec inner = ((x, y) as c: coord, {x: vx, y: vy} as v: velocity, trajectory) => {
      //      j`c = $c`->log
      if isHit(c) {
        Hit(c, trajectory)
      } else if isOutOfRange(c) {
        Miss(Array.concat(trajectory, [c]))
      } else {
        inner((x + vx, y + vy), velocityNext(v), Array.concat(trajectory, [c]))
      }
    }

    inner((0, 0), v0, [])
  }
}

@@warning("-8")
open TrickShot
let parse = (data): target => {
  module Str = Js.String2
  let [x_str, y_str] = data->Str.replace("target area: ", "")->Str.split(", ")
  let [x_min, x_max] =
    x_str->Str.replace("x=", "")->Str.split("..")->Array.map(x => x->Int.fromString->Option.getExn)
  let [y_min, y_max] =
    y_str->Str.replace("y=", "")->Str.split("..")->Array.map(y => y->Int.fromString->Option.getExn)
  {x_min: x_min, x_max: x_max, y_min: y_min, y_max: y_max}
}

let solvePart1 = data => {
  let t = data->parse
  let v = {x: 6, y: 9}
  launch(v, t)->dump->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
