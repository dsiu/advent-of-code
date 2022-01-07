open Belt
open Utils
let log = Js.Console.log

module LanternFish = {
  type state = int
  type t = Decr(state) | Respawn(state)

  let make = s => {
    s == 6 ? Respawn(s) : Decr(s)
  }

  let makeRespawned = () => Decr(8)

  let getNextState = t => {
    switch t {
    | Decr(0) => Respawn(6)
    | Decr(s)
    | Respawn(s) =>
      Decr(s - 1)
    }
  }

  // isRespawned returns true when last state is 0; ie: current state = 6
  let isRespawned = t => {
    switch t {
    | Respawn(_) => true
    | _ => false
    }
  }

  let toString = t => {
    switch t {
    | Decr(s)
    | Respawn(s) =>
      s->Int.toString
    }
  }
}

let rec run = (fs, days) => {
  Js.log2("days", days)
  //  Js.log2("fs", fs->Array.map(LanternFish.toString))
  switch days {
  | 0 => fs
  | d => {
      let next = fs->Array.map(LanternFish.getNextState)
      Array.concat(
        next,
        next->Array.keepMap(f => {
          f->LanternFish.isRespawned ? LanternFish.makeRespawned()->Some : None
        }),
      )->run(d - 1)
    }
  }
}

let parse = data =>
  data
  ->Js.String2.trim
  ->Js.String2.split(",")
  ->Array.map(s => s->Int.fromString->Option.getExn->LanternFish.make)

let solvePart1 = data => {
  let days = 80
  let result = data->parse->run(days)
  result->Array.length
}

let solvePart2 = data => {
  let days = 256
  let result = data->parse->run(days)
  result->Array.length
}
