open Belt
open Utils

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

  let isOutOfRange_ = ((x, y): coord, {x_min: _, x_max, y_min, y_max: _}: target) => {
    //    j`isOutOfRange $x, $y, {$x_min, $x_max, $y_min, $y_max}`->log
    x > x_max || y < y_min
  }

  let isOUtOfRange = t_init => isOutOfRange_(_, t_init)

  type trajectory = array<coord>

  type launchResult =
    | Hit(coord, trajectory)
    | Miss(trajectory)

  let toString = (r: launchResult) => {
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

  // returns results that hits target
  //
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

  let iterate = (~vx_start, ~vx_end, ~vy_start, ~vy_end, target) => {
    let result = ref(([]: array<(velocity, launchResult)>))
    for x in vx_start to vx_end {
      for y in vy_start to vy_end {
        let v = {x: x, y: y}
        let r = launch(v, target)
        switch r {
        | Hit(_) => result := Array.concat(result.contents, [(v, r)])
        | Miss(_) => ()
        }
      }
    }
    result.contents
  }

  let part1 = (~vx_start, ~vx_end, ~vy_start, ~vy_end, target) => {
    let getMaxY = Array.reduce(_, Js.Int.min, (acc, c) => {c->snd > acc ? c->snd : acc})

    let launch_results = iterate(~vx_start, ~vx_end, ~vy_start, ~vy_end, target)

    let r_max_height = launch_results->Array.reduce(
      (Js.Int.min, launch_results[0]->Option.getExn),
      ((y_max, _) as acc, (v, r')) => {
        switch r' {
        | Hit(_, traj) => {
            let y = getMaxY(traj)
            y > y_max ? (y, (v, r')) : acc
          }
        | Miss(_) => acc
        }
      },
    )

    r_max_height
  }

  let part2 = (~vx_start, ~vx_end, ~vy_start, ~vy_end, target) => {
    iterate(~vx_start, ~vx_end, ~vy_start, ~vy_end, target)->Array.size
  }
}

@@warning("-8")
open TrickShot
let parse = (data): target => {
  module Str = Js.String2
  let [x_str, y_str] = data->Str.replace("target area: ", "")->Str.split(", ")
  let [x_min, x_max] =
    x_str->Str.replace("x=", "")->Str.split("..")->Array.map(x => x->intFromStringExn)
  let [y_min, y_max] =
    y_str->Str.replace("y=", "")->Str.split("..")->Array.map(y => y->intFromStringExn)
  {x_min: x_min, x_max: x_max, y_min: y_min, y_max: y_max}
}

let solvePart1 = data => {
  let t = data->parse
  let (max_y, ({x: _vx, y: _vy}, _r)) = part1(
    ~vx_start=1,
    ~vx_end=t.x_max - 1,
    ~vy_start=0,
    ~vy_end=-t.y_min - 1,
    t,
  )

  //  j`max_y = $max_y`->log
  //  j`velocity = $vx, $vy`->log

  //  r->toString->log->log
  max_y
}

let solvePart2 = data => {
  let t = data->parse
  let vy_start = -Js.Math.abs_int(t.y_min)
  let vy_end = -vy_start
  part2(~vx_start=0, ~vx_end=t.x_max, ~vy_start, ~vy_end, t)
}
