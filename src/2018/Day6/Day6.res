@@warning("-27")

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
  let makeFromArray = xs => {
    make(~x=xs->Array.get(0)->Option.getExn, ~y=xs->Array.get(1)->Option.getExn)
  }

  // Coords utils
  let findXY = (f, init, xs) => {
    xs->Array.reduce(make(~x=init, ~y=init), (a, c) => {
      make(~x=f(a.x, c.x), ~y=f(a.y, c.y))
    })
  }
  let maxXY = findXY(Js.Math.max_int, Js.Int.min)
  let minXY = findXY(Js.Math.min_int, Js.Int.max)

  let dist = (a: t, b: t) => {
    (b.x - a.x)->Js.Math.abs_int + (b.y - a.y)->Js.Math.abs_int
  }

  let parse = l => {
    l
    ->Js.String2.trim
    ->Js.String2.split(",")
    ->Array.map(x => {
      x->Js.String2.trim->Int.fromString->Option.getExn
    })
    ->makeFromArray
  }

  let parseCoords = Array.map(_, parse)
}

module LandingMap = {
  type dist = int
  // type cell = Map.Int.t<dist>
  type cell = int
  type row = Map.Int.t<cell>
  type col = Map.Int.t<row>
  type w = int
  type h = int

  type t = {
    pins: Map.Int.t<Coord.t>, // Int=location id;
    grid: col,
    w: w,
    h: h,
    maxBound: Coord.t,
    minBound: Coord.t,
  }

  let w = t => t.w
  let h = t => t.h
  let grid = t => t.grid

  let distsFromPins = (at, pins) => {
    //    pins->Map.Int.reduce(Map.Int.empty, (a, k, v) => {
    //      a->Map.Int.set(k, Coord.dist(at, v))
    //    })
    pins->Map.Int.map(v => {Coord.dist(at, v)})
  }

  let findMinDists = Map.Int.reduce(_, Js.Int.max, (a, k, v) => {
    v < a ? v : a
  })

  let keepOnly = (~value, xs) => xs->Map.Int.keep((k, v) => {v === value})

  let makeCellShortest = (at, pins) => {
    //    " "->log

    //    `at ${at->Coord.x->Int.toString},${at->Coord.y->Int.toString}`->log

    let dists = at->distsFromPins(pins)
    //    dists->dump_mapInt_of_int

    let minDist = dists->findMinDists
    //    `minDist:${minDist->Int.toString}`->log

    //    (`from ${a.x->Int.toString},${a.y->Int.toString} to ${b.x->Int.toString},${b.y->Int.toString} = ${d->Int.toString}`)->log
    let onlyMins = dists->keepOnly(~value=minDist)
    //    "onlyMins: "->log
    //    onlyMins->dump_mapInt_of_int

    open Map.Int
    assert (onlyMins->size > 0)

    let ret = onlyMins->size > 1 ? -1 : onlyMins->reduce(Js.Int.min, (a, k, v) => k)

    //    ret->log
    ret
  }

  let alloc = t => {
    // "alloc"->log
    // (t.w, t.h)->log
    let filled = Array.range(0, t.w)->Array.reduce(Map.Int.empty, (a, x) => {
      a->Map.Int.set(
        x,
        Array.range(0, t.h)->Array.reduce(Map.Int.empty, (b, y) => {
          // b->Map.Int.set(y, Map.Int.empty)
          b->Map.Int.set(y, -99)
        }),
      )
    })
    {...t, grid: filled}
  }

  let fill = t => {
    open Map.Int
    let filled = t.grid->reduce(Map.Int.empty, (a, kx, x) => {
      a->set(
        kx,
        x->reduce(Map.Int.empty, (a, ky, y) => {
          a->set(ky, makeCellShortest(Coord.make(~x=kx, ~y=ky), t.pins))
        }),
      )
    })
    {...t, grid: filled}
  }

  let make = xs => {
    open Map.Int
    let maxBound = xs->Coord.maxXY
    let minBound = xs->Coord.minXY
    maxBound->log
    minBound->log
    // s->log
    let pinsMap = xs->Array.reduceWithIndex(empty, (a, x, i) => {a->set(i, x)})
    pinsMap->dump_mapInt_of(c => c->Coord.x->Int.toString ++ " " ++ c->Coord.y->Int.toString)
    {
      pins: pinsMap,
      grid: empty,
      w: maxBound->Coord.x,
      h: maxBound->Coord.y,
      maxBound: maxBound,
      minBound: minBound,
    }
    ->alloc
    ->fill
  }

  let countCellWith = (~pinId, t) => {
    open Map.Int
    t.grid->reduce(0, (a, kx, x) => {
      a + x->keepOnly(~value=pinId)->size
    })
  }

  let getNonInfPin = t => {
    let {pins, w, h, maxBound, minBound} = t
    pins->Map.Int.keep((k, v) => {
      !(v.x === maxBound.x || v.x === minBound.x || v.y === maxBound.y || v.y === minBound.y)
    })
  }

  let findLandingAreasOfPins = t => {
    //    t->getNonInfLoc->Map.Int.mapWithKey((k,v) => {
    //      t->countCellWith(~value=k)
    //    })
    t.pins->Map.Int.mapWithKey((k, v) => {
      t->countCellWith(~pinId=k)
    })
  }

  let getMaxArea = m => {
    open Map.Int
    m->reduce(0, (a, k, v) => {Js.Math.max_int(a, v)})
  }

  let numToChar = xs => {
    xs->Array.map(x => {
      switch x {
      | -1 => "."
      | c => Js.String2.fromCharCode(97 + c)
      }
    })
  }

  let dump = t => {
    "dump"->log
    open Map.Int
    "x, y, v"->log
    t
    ->grid
    ->forEach((kx, vx) => {
      vx->valuesToArray->numToChar->Js.Console.logMany
    })
  }
}

let solvePart1 = data => {
  let map = data->Js.String2.split("\n")->Coord.parseCoords->LandingMap.make
  let areas = map->LandingMap.findLandingAreasOfPins

  //  map->LandingMap.dump

  ` ========= landing areas (size = ${areas->Map.Int.size->Int.toString})`->log
  areas->dump_mapInt_of_int

  let targetPins = map->LandingMap.getNonInfPin
  ` ======== target pins (size = ${targetPins->Map.Int.size->Int.toString})`->log
  targetPins->dump_mapInt_of(c => {c->Coord.x->Int.toString ++ " " ++ c->Coord.y->Int.toString})

  open Map.Int
  let maxArea =
    areas
    ->keep((k, v) => {
      targetPins->has(k)
    })
    ->Map.Int.reduce(Js.Int.min, (a, k, v) => {
      v > a ? v : a
    })

  " ======== answer"->log

  //  `targetPin = ${targetPin->Int.toString}`->log
  `maxArea = ${maxArea->Int.toString}`->log
}
