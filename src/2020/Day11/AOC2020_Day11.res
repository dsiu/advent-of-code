open Belt
let log = Js.Console.log
//open Utils

module SeatMap = {
  exception InvalidStatus(string)

  module SeatStatus = {
    type t = [#L | #"." | #"#"]

    let make = c => {
      switch c {
      | "." => #"."
      | "L" => #L
      | "#" => #"#"
      | _ => raise(InvalidStatus(c))
      }
    }
  }

  type t = Array2D.t<SeatStatus.t>

  let isValidCoord = ((x, y), ~lenX, ~lenY) => {
    x >= 0 && x <= lenX - 1 && y >= 0 && y <= lenY - 1
  }

  let adjCoords = ((x, y)) => {
    [
      (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1),
    ]
  }

  let getAdjacents = (t, (x, y)) => {
    let lenX = t->Array2D.lengthX
    let lenY = t->Array2D.lengthY

    let validAdjs = (x, y)->adjCoords->Array.keep(isValidCoord(_, ~lenX, ~lenY))

    validAdjs->Array.map(t->Array2D.get)
  }

  let isSeatEq = (s, toBe) => {
    Js.log2("s", s)
    Js.log2("toBe", toBe)
    s->Option.isSome && s->Option.getExn === toBe
  }

  let countSeat = (xs, seatStatus: SeatStatus.t) => {
    xs->Array.keep(isSeatEq(_, seatStatus))->Array.length
  }

  let countEmptySeat = countSeat(_, #L)
  let countFloor = countSeat(_, #".")
  let countOccupiedSeat = countSeat(_, #"#")

  let make = (xs: array<string>) => {
    let x = xs->Array.getExn(0)->Js.String2.length
    let y = xs->Array.length
    let ret = Array2D.make((x, y), #".")

    open Utils
    xs->Array.forEachWithIndex((y, ys) => {
      ys
      ->splitChars
      ->Array.forEachWithIndex((x, c) => {
        ret->Array2D.set((x, y), c->SeatStatus.make)->ignore
      })
    })

    ret
  }

  let dump = t => {
    for y in 0 to t->Array2D.lengthY - 1 {
      t->Array2D.getYEquals(y)->Option.getExn->Utils.join->log
    }
  }
}

let parse = data => {
  let parsed = data->Js.String2.split("\n")->Array.map(Js.String2.trim)
  SeatMap.make(parsed)
}

let solvePart1 = data => {
  let seats = data->parse
  let adj = seats->SeatMap.getAdjacents((2, 0))
  adj->log
  adj->SeatMap.countEmptySeat->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
