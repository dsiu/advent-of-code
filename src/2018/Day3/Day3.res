let data = Day3_Data.data
open Belt

module type Claim = {
  type id = int

  type t = {
    id: id,
    x: int,
    y: int,
    w: int,
    h: int,
  }

  let id: t => id
  let x: t => int
  let y: t => int
  let w: t => int
  let h: t => int

  let maxX: t => int
  let maxY: t => int

  let parseLine: string => array<string>
  let make: (~id: id, ~x: int, ~y: int, ~w: int, ~h: int) => t
  let makeClaim: string => t
}

module Claim: Claim = {
  type id = int

  type t = {
    id: id,
    x: int,
    y: int,
    w: int,
    h: int,
  }

  let id = t => t.id
  let x = t => t.x
  let y = t => t.y
  let w = t => t.w
  let h = t => t.h

  let maxX = t => t->x + t->w
  let maxY = t => t->y + t->h

  // #1 @ 669,271: 17x11
  let claimRe = %re("/#(\d+)\s+@\s+(\d+),(\d+):\s(\d+)x(\d+)/i")

  // parse a string and produce array of params for claim
  let parseLine = s => {
    switch s->Js.String.trim |> Js.Re.exec_(claimRe) {
    | Some(x) => Js.Re.captures(x)->Array.map(x => Js.Nullable.toOption(x)->Option.getExn)
    | None => []
    }
  }

  let make = (~id: int, ~x, ~y, ~w, ~h) => {id: id, x: x, y: y, w: w, h: h}

  // given a raw data line, produce a Claim.t
  let makeClaim = l => {
    l
    ->parseLine
    ->(
      xs =>
        make(
          ~id=xs->Array.get(1)->Option.getExn->int_of_string,
          ~x=xs->Array.get(2)->Option.getExn->int_of_string,
          ~y=xs->Array.get(3)->Option.getExn->int_of_string,
          ~w=xs->Array.get(4)->Option.getExn->int_of_string,
          ~h=xs->Array.get(5)->Option.getExn->int_of_string,
        )
    )
  }
}

module Claims = {
  type t = array<Claim.t>

  let findMax = (t, f): int => {
    t->Array.reduce(0, (acc, x) => {
      f(x) > acc ? f(x) : acc
    })
  }

  let findMaxX = t => t->findMax(Claim.maxX)
  let findMaxY = t => t->findMax(Claim.maxY)

  let make = (lines): t => {
    lines |> Js.Array.map(Claim.makeClaim)
  }
}

@@warning("-27")
module Fabric = {
  type w = int
  type h = int
  type cols = MutableMap.Int.t<array<Claim.id>>
  type matrix = Map.Int.t<cols>

  type t = {
    w: w,
    h: h,
    matrix: matrix,
  }

  let w = t => t.w
  let h = t => t.h
  let matrix = t => t.matrix

  let make = (~w, ~h) => {
    w: w,
    h: h,
    matrix: Array.range(0, w)->Array.reduce(Map.Int.empty, (acc, i) =>
      Map.Int.set(acc, i, MutableMap.Int.make())
    ),
  }

  let dump = t => {
    t
    ->matrix
    ->Map.Int.forEach((x, col) => {
      col->MutableMap.Int.forEach((y, vs) => {
        Js.Console.log(`x:${x->string_of_int} y:${y->string_of_int}`)
        vs->Array.forEach(v => Js.Console.log("  " ++ v->string_of_int))
      })
    })
  }

  let twoOrMore = x => x >= 2
  let oneOrMore = x => x >= 1
  let isOne = x => x === 1

  let addPoint = (t, ~x, ~y, p) => {
    t
    ->matrix
    ->Map.Int.get(x)
    ->Option.getExn
    ->MutableMap.Int.update(y, a => {
      switch a {
      | Some(a) => Some(a->Array.concat([p]))
      | None => Some([p])
      }
    })
    t
  }

  let getPoint = (t, ~x, ~y): option<array<Claim.id>> => {
    t->matrix->Map.Int.get(x)->Option.getExn->MutableMap.Int.get(y)
  }

  let fill = (t, f) => {
    Array.range(0, t->w)->Array.reduce(t, (acc, x) =>
      Array.range(0, t->h)->Array.reduce(t, (acc, y) => acc->addPoint(~x, ~y, f(x, y)))
    )
  }

  let claimAreaIter = (c: Claim.t, t, f) => {
    // should use each
    Array.range(c->Claim.x, c->Claim.x + c->Claim.w - 1)->Array.reduce(t, (acc, x) =>
      Array.range(c->Claim.y, c->Claim.y + c->Claim.h - 1)->Array.reduce(t, (acc, y) =>
        f(acc, ~x, ~y, c)
      )
    )
  }

  // add claim
  let addClaimIdToPoint = (t, ~x, ~y, c: Claim.t) => {
    t->addPoint(~x, ~y, c->Claim.id)
  }

  let addClaim = (t, c: Claim.t) => {
    claimAreaIter(c, t, addClaimIdToPoint)
  }

  @@warning("-27")
  let getClaimIdFromPointIf = (t, c, ~x, ~y, c: Claim.t) => {
    let point = t->getPoint(~x, ~y)
    let len = point->Option.getExn->Array.length
    isOne(len) ? Some(point) : None
  }

  // count nonoverlap
  let getClaimIdsFromArea = (t, c: Claim.t) => {
    let cids =
      Array.range(c->Claim.x, c->Claim.x + c->Claim.w - 1)
      ->Array.reduce([], (accX, x) =>
        Array.range(c->Claim.y, c->Claim.y + c->Claim.h - 1)->Array.reduce([], (accY, y) =>
          switch t->getPoint(~x, ~y) {
          | Some(
              p,
            ) => //              Js.Console.log(`x: ${x->string_of_int} y: ${y->string_of_int}`)
            //              Js.Console.log("point")
            //              Js.Console.log(p)
            accY->Array.concat([p])
          | None => accY
          }
        ) |> Array.concat(accX)
      )
      ->Utils.flattenArray

    //    Js.Console.log("cids")
    //    Js.Console.log(cids)

    c->Claim.w * c->Claim.h == cids->Array.length ? Some(cids[0]) : None
  }

  // returns [Claim.id]
  let countNonOverlapClaim = (t, xs: Claims.t) => {
    let r = (t, acc, c) => {
      let cid = t->getClaimIdsFromArea(c)
      switch cid {
      | Some(c) => acc->Array.concat([c])
      | None => acc
      }
    }
    let reducer = r(t)
    xs->Array.reduce([], reducer)
  }

  @@warning("-27")
  // returns count:int
  let countOverlap = (t, p) => {
    t
    ->matrix
    ->Map.Int.reduce(0, (acc, x, col) => {
      acc +
      col->MutableMap.Int.reduce(0, (acc, y, vs) => {
        p(vs->Array.length) ? acc + 1 : acc
      })
    })
  }
}

// data -> Js.String2.split("\n") -> allClaim -> Js.Console.log

// let size_x = data->Js.String2.split("\n")->Claims.make->Claims.findMaxX
// let size_y = data->Js.String2.split("\n")->Claims.make->Claims.findMaxY
//
// let fab = Fabric.make(~w=size_x, ~h=size_y)

let solvePart1 = () => {
  let allClaims = data->Js.String2.split("\n")->Claims.make
  let fab = Fabric.make(~w=allClaims->Claims.findMaxX, ~h=allClaims->Claims.findMaxY)
  let fab = allClaims->Belt.Array.reduce(fab, (acc, i) => acc->Fabric.addClaim(i))
  fab->Fabric.countOverlap(Fabric.twoOrMore)
}

let solvePart2 = () => {
  let allClaims = data->Js.String2.split("\n")->Claims.make
  let fab = Fabric.make(~w=allClaims->Claims.findMaxX, ~h=allClaims->Claims.findMaxY)
  let fab = allClaims->Belt.Array.reduce(fab, (acc, i) => acc->Fabric.addClaim(i))
  fab->Fabric.countNonOverlapClaim(allClaims)
}

@@warning("-26")
let solvePart2Demo =
  () == {
      let test_line1 = "#3 @ 1,3: 4x4"
      let test_line2 = "#7 @ 3,1: 4x4"
      let test_line3 = "#11 @ 5,5: 2x2"
      let allClaims = [test_line1, test_line2, test_line3]->Claims.make
      let w = allClaims->Claims.findMaxX
      let h = allClaims->Claims.findMaxY
      let test_fab = Fabric.make(~w, ~h)
      let test_fab = allClaims->Belt.Array.reduce(test_fab, (acc, i) => {
        acc->Fabric.addClaim(i)
      })
      let result = test_fab->Fabric.countNonOverlapClaim(allClaims)
    }
  // result->Js.Console.log

// solvePart2() |> Js.Console.log
