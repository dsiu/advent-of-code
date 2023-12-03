open Belt
//open Utils
let log = Js.Console.log

module Log = unpack(JS_Debug.make(__MODULE__))

module LanternFish = {
  type state = int
  type t = Decr(state) | Respawn(state)

  let make = s => {
    s == 6 ? Respawn(s) : Decr(s)
  }

  let makeRespawned = () => Decr(8)

  let getNextState = (. t) => {
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
      s->Belt.Int.toString
    }
  }
}

let rec runDumb = (fs, days) => {
  Js.log2("days", days)
  Log.debug("run " ++ `days`)
  Log.debug("fs size " ++ fs->Array.length->Belt.Int.toString)
  //  Js.log2("fs", fs->Array.map(LanternFish.toString))
  switch days {
  | 0 => fs
  | d => {
      Log.debug("get next state")
      let next = fs->Array.mapU(LanternFish.getNextState)
      Log.debug("get next state done")

      Log.debug("get spawned")
      let spawn = next->Array.keepMapU((. f) => {
        f->LanternFish.isRespawned ? LanternFish.makeRespawned()->Some : None
      })

      Log.debug("get spawned done")
      let result = Array.concat(next, spawn)
      Log.debug("done result concat")

      runDumb(result, d - 1)
    }
  }
}
module BigInt = RescriptCore.BigInt
let big_zero = BigInt.fromInt(0)
let big_one = BigInt.fromInt(1)
let add = BigInt.add

let runSmart = (fs, days) => {
  let counts = Array.make(9, big_zero)
  fs->Array.forEach(n => {
    counts->Array.setExn(n, add(counts->Array.getExn(n), big_one))
  })

  let rec run = (counts, days) => {
    //    Js.log2("days", days)

    switch days {
    | 0 => counts
    | d => {
        let newCount = counts[0]->Option.getExn
        let result = counts->Array.sliceToEnd(1)
        result->Array.setExn(6, add(result->Array.getExn(6), newCount))
        Array.concat(result, [newCount])->run(d - 1)
      }
    }
  }

  run(counts, days)
  ->Belt.Array.reduce(BigInt.fromInt(0), (total, x) => {
    BigInt.add(x, total)
  })
  ->BigInt.toString
}

let parse = data =>
  data
  ->Js.String2.trim
  ->Js.String2.split(",")
  ->Array.map(s => s->Belt.Int.fromString->Belt.Option.getExn)

let solvePart1 = data => {
  let days = 80
  //  let result = data->parse->Array.map(LanternFish.make)->runDumb(days)
  let result = data->parse->runSmart(days)

  //  let result = data->parse->runLinear(days)
  result
}

let solvePart2 = data => {
  let days = 256
  let result = data->parse->runSmart(days)
  result
}
