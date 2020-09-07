open Belt
open Utils
let log = Js.Console.log
let data = Day6_Data.data
let testData = Day6_Data_Test.data

module Coord = {
  type t = {x: int, y: int}

  let x = t => t.x
  let y = t => t.y

  let make = (~x, ~y) => {x: x, y: y}
  let makeFromArray = xs => {x: xs->Array.get(0)->Option.getExn, y: xs->Array.get(1)->Option.getExn}

  let maxXY = xs => {
    xs->Array.reduce(make(~x=0, ~y=0), (a, c) => {
      switch (a.x < c.x, a.y < c.y) {
      | (true, false) => make(~x=c.x, ~y=a.y)
      | (false, true) => make(~x=a.x, ~y=c.y)
      | (true, true) => make(~x=c.x, ~y=c.y)
      | (false, false) => a
      }
    })
  }

  let dist = (a: t, b: t) => {
    (b.x - a.x)->Js.Math.abs_int + (b.y - a.y)->Js.Math.abs_int
  }
}

module Map = {
  type dist = int
//  type cell = Map.Int.t<dist>
  type cell = int
  type row = Map.Int.t<cell>
  type col = Map.Int.t<row>
  type w = int
  type h = int

  type t = {
    locs: array<Coord.t>,
    grid: col,
    w: w,
    h: h,
  }

  let w = t => t.w
  let h = t => t.h
  let grid = t => t.grid

  let makeCell_full = (at, locs) => {
    locs->Array.reduceWithIndex(Map.Int.empty, (a, l, i) => {
      a->Map.Int.set(i, Coord.dist(at, l))
    })
  }

   let makeCellShortest = (at, locs) => {
        "makeCellShortest" -> log

    open Map.Int
     let allDists = makeCell_full(at,locs)
     let minDist = allDists->reduce(Js.Int.max, (a, k, v)=>{
        v < a ? v : a
     })

     at -> log
     minDist ->log

     let (min, foundMinDist) = allDists->reduce((minDist, []), (a, k, v) => {
        let (minDist, acc) = a
        v == minDist ? (minDist, acc->Array.concat([v])) : a
     })

     foundMinDist->Array.length >= 1 ? -1 : foundMinDist->Array.get(0)->Option.getExn
  }


  let alloc = t => {
    "alloc"->log
    (t.w, t.h)->log
    let filled = Array.range(0, t.w - 1)->Array.reduce(Map.Int.empty, (a, x) => {
      a->Map.Int.set(x, Array.range(0, t.h - 1)->Array.reduce(Map.Int.empty, (b, y) => {
//          b->Map.Int.set(y, Map.Int.empty)
          b->Map.Int.set(y, -1)
        }))
    })
    {...t, grid: filled}
  }

  let fill = t => {
    open Map.Int
    let filled = t->alloc->grid->reduce(Map.Int.empty, (a, kx, x) => {
      a->set(kx, x->reduce(Map.Int.empty, (a, ky, y) => {
          a->set(ky, makeCellShortest(Coord.make(~x=kx, ~y=ky), t.locs))
        }))
    })
    log("fill")
    filled->log
    {...t, grid: filled}
  }

  let make = xs => {
    let s = xs->Coord.maxXY
//    s->log
    {locs: xs, grid: Map.Int.empty, w: s->Coord.x, h: s->Coord.y}->fill
  }

  let dump = t => {
    "dump"->log
    open Map.Int
    t->grid->forEach((kx, vx) => {
      kx->log
      vx->forEach((ky, vy) => {
        ky->log
        (kx, ky, vy)->log
//        vy->Utils.map_int_dump

      })
    })
  }
}

let parse = l => {
  l->Js.String2.trim->Js.String2.split(",")->Array.map(x => {
    x->Js.String2.trim->Int.fromString->Option.getExn
  })->Coord.makeFromArray
}
