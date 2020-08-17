let data = Day3_Data.data

module Claim = {
  type id = int
  type x = int
  type y = int
  type w = int
  type h = int

  type t = {
    id: id,
    x: x,
    y: y,
    w: w,
    h: h,
  }

  let id = t => t.id
  let x = t => t.x
  let y = t => t.y
  let w = t => t.w
  let h = t => t.h

  let maxX = t => t->x + t->w
  let maxY = t => t->y + t->h

  let make = (~id: int, ~x, ~y, ~w, ~h) => {id: id, x: x, y: y, w: w, h: h}
}

// #1 @ 669,271: 17x11
let claimRe = %re("/#(\d+)\s+@\s+(\d+),(\d+):\s(\d+)x(\d+)/i")

// parse a string and produce array of params for claim
let parseLine = s => {
  switch s->Js.String.trim->Js.Re.exec(claimRe) {
  | Some(x) => Js.Re.captures(x)->Belt.Array.map(x => {
      switch Js.Nullable.toOption(x) {
      | Some(y) => y
      | None => ""
      }
    })
  | None => []
  }
}

// given a raw data line, produce a Claim.t
let makeClaim = (x): Claim.t => {
  x
  ->parseLine
  ->(
    xs =>
      Claim.make(
        ~id=xs->Array.get(1)->int_of_string,
        ~x=xs->Array.get(2)->int_of_string,
        ~y=xs->Array.get(3)->int_of_string,
        ~w=xs->Array.get(4)->int_of_string,
        ~h=xs->Array.get(5)->int_of_string,
      )
  )
}

let allClaim = (lines): array<Claim.t> => {
   lines |> Js.Array.map(makeClaim)
}

let findMax = (xs, f): int => {
  xs->Belt.Array.reduce(0, (acc, x) => {
    f(x) > acc ? f(x) : acc
  })
}

let findMaxX = findMax(_, Claim.maxX)
let findMaxY = findMax(_, Claim.maxY)

module Fabric = {
  type w = int
  type h = int

  type t = {
    w: w,
    h: h,
  }

  let make = (~w, ~h) => {w: w, h: h}
  let w = t => t.w
  let h = t => t.h
}

//data -> Js.String2.split("\n") -> allClaim -> Js.Console.log

let size_x = data -> Js.String2.split("\n") -> allClaim -> findMaxX
let size_y = data -> Js.String2.split("\n") -> allClaim -> findMaxY

let fab = Fabric.make(~w=size_x, ~h=size_y)

