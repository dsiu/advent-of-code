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

  let findXY = (f, init, xs) => {
    xs->Array.reduce(make(~x=init, ~y=init), (a, c) => {
      make(~x=f(a.x, c.x), ~y=f(a.y, c.y))
    })
  }
  let maxXY = findXY(Js.Math.max_int, Js.Int.min)
  let minXY = findXY(Js.Math.min_int, Js.Int.max)

  let dist = (a: t, b: t) => {
    let d = (b.x - a.x)->Js.Math.abs_int + (b.y - a.y)->Js.Math.abs_int
//    (`from ${a.x->Int.toString},${a.y->Int.toString} to ${b.x->Int.toString},${b.y->Int.toString} = ${d->Int.toString}`)->log
    d
  }

  let parse = l => {
    l->Js.String2.trim->Js.String2.split(",")->Array.map(x => {
      x->Js.String2.trim->Int.fromString->Option.getExn
    })->makeFromArray
  }

  let parseCoords = Array.map(_, parse)
}

module Map = {
  type dist = int
  // type cell = Map.Int.t<dist>
  type cell = int
  type row = Map.Int.t<cell>
  type col = Map.Int.t<row>
  type w = int
  type h = int

  type t = {
    locs: Map.Int.t<Coord.t>,
    grid: col,
    w: w,
    h: h,
    maxXY: Coord.t,
    minXY: Coord.t,
  }

  let w = t => t.w
  let h = t => t.h
  let grid = t => t.grid

  let distsFromLocs = (at, locs) => {
    locs->Map.Int.reduce(Map.Int.empty, (a, k, v) => {
      a->Map.Int.set(k, Coord.dist(at, v))
    })
  }

  let findMinDists = Map.Int.reduce(_, Js.Int.max, (a, k, v) => {
    v < a ? v : a
  })

  let keepOnly = (~value, xs) => xs->Map.Int.keep((k, v) => {v === value})

  let makeCellShortest = (at, locs) => {
    "makeCellShortest"->log
    open Map.Int
    let dists = distsFromLocs(at, locs)
    " " -> log
    "dists --> "->log
//    dists->dump_mapInt_of_int
    let minDist = dists->findMinDists
    (`at ${at.x->Int.toString},${at.y->Int.toString} | minDist:${minDist->Int.toString}`)->log
    //    (`from ${a.x->Int.toString},${a.y->Int.toString} to ${b.x->Int.toString},${b.y->Int.toString} = ${d->Int.toString}`)->log
    let onlyMins = dists->keepOnly(~value=minDist)
    "onlyMins -->" ->log
    onlyMins->dump_mapInt_of_int

    assert(onlyMins->size > 0)
    onlyMins->size > 1 ? -1 : onlyMins->reduce(Js.Int.min, (a, k, v) => k)
  }

  let alloc = t => {
    // "alloc"->log
    // (t.w, t.h)->log
    let filled = Array.range(0, t.w - 1)->Array.reduce(Map.Int.empty, (a, x) => {
      a->Map.Int.set(x, Array.range(0, t.h - 1)->Array.reduce(Map.Int.empty, (b, y) => {
          // b->Map.Int.set(y, Map.Int.empty)
          b->Map.Int.set(y, -1)
        }))
    })
    {...t, grid: filled}
  }

  let fill = t => {
//    "fill"->log
//    t->log
    open Map.Int
    let filled = t.grid->reduce(Map.Int.empty, (a, kx, x) => {
      a->set(kx, x->reduce(Map.Int.empty, (a, ky, y) => {
          a->set(ky, makeCellShortest(Coord.make(~x=kx, ~y=ky), t.locs))
        }))
    })
    {...t, grid: filled}
  }

  let make = xs => {
    open Map.Int
    let maxXY = xs->Coord.maxXY
    let minXY = xs->Coord.minXY
    maxXY->log
    minXY->log
    // s->log
    let locsMap = xs->Array.reduceWithIndex(empty, (a, x, i) => {a->set(i, x)})
    dump_mapInt_of(c => c->Coord.x->Int.toString ++ " " ++ c->Coord.y->Int.toString)(locsMap)
    {
      locs: locsMap,
      grid: empty,
      w: maxXY->Coord.x - 1,
      h: maxXY->Coord.y - 1,
      maxXY: maxXY,
      minXY: minXY,
    }->alloc->fill
  }

  let countCellWith = (~value, t) => {
    open Map.Int
    t.grid->reduce(0, (a, kx, x) => {
      a + x->keepOnly(~value)->size
    })
  }

  let getNonInfLoc = t => {
    let {locs, w, h, minXY, maxXY} = t
    "getNonInfLoc"->log
    t->log
    locs->Map.Int.keep((k, v) => {
      v->log
      !(v.x === maxXY.x || v.x === minXY.x || v.y === minXY.y || v.y === minXY.y)
    })
  }

  let findAreas = t => {
//    t->getNonInfLoc->Map.Int.mapWithKey((k,v) => {
//      t->countCellWith(~value=k)
//    })
    t.locs->Map.Int.mapWithKey((k, v) => {
      t->countCellWith(~value=k)
    })
  }

  let getMaxArea = m => {
    open Map.Int
    m->reduce(0, (a, k, v) => {Js.Math.max_int(a, v)})
  }

  let dump = t => {
    "dump"->log
    open Map.Int
   "x, y, v"->log
    t->grid->forEach((kx, vx) => {
      vx->forEach((ky, vy) => {

        (kx, ky, vy)->log
        // vy->Utils.map_int_dump
      })
    })
  }
}
