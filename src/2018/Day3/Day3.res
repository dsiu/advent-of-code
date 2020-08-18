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
  let makeClaim = x => {
    x
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
    matrix: Array.range(0, h)->Array.reduce(Map.Int.empty, (acc, i) =>
      Map.Int.set(acc, i, MutableMap.Int.make())
    ),
  }

  let addPoint = (t, ~x, ~y, ~p) => {
    t->matrix->Map.Int.get(x)->Option.getExn->MutableMap.Int.update(y, a => {
      switch a {
      | Some(a) => Some(a->Array.concat([p]))
      | None => Some([p])
      }
    })
    t
  }

  let getPoint = (t, ~x, ~y): array<Claim.id> => {
    t->matrix->Map.Int.get(x)->Option.getExn->MutableMap.Int.get(y)->Option.getExn
  }

  let fill = (t, f) => {
    Array.range(0, t->w)->Array.reduce(t, (acc, x) =>
      Array.range(0, t->h)->Array.reduce(t, (acc, y) => acc->addPoint(~x, ~y, ~p=f(x, y)))
    )
  }

  let addClaim = (t, c: Claim.t) => {
    Array.range(c->Claim.x, c->Claim.x + c->Claim.w)->Array.reduce(t, (acc, x) =>
      Array.range(c->Claim.y, c->Claim.y + c->Claim.h)->Array.reduce(t, (acc, y) =>
        acc->addPoint(~x, ~y, ~p=c->Claim.id)
      )
    )
  }
}

// data -> Js.String2.split("\n") -> allClaim -> Js.Console.log

let size_x = data->Js.String2.split("\n")->Claims.make->Claims.findMaxX
let size_y = data->Js.String2.split("\n")->Claims.make->Claims.findMaxY

let fab = Fabric.make(~w=size_x, ~h=size_y)
