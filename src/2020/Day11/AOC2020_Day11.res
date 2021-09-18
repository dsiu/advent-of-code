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

  let isValidCoord = ((x, y), ~len_x, ~len_y) => {
    x >= 0 && x <= len_x - 1 && y >= 0 && y <= len_y - 1
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
    let len_x = t->Array2D.lengthX
    let len_y = t->Array2D.lengthY

    (x, y)->adjCoords->Array.keep(isValidCoord(_, ~len_x, ~len_y))->Array.map(t->Array2D.get)
  }

  let isSeatEq = (s: option<SeatStatus.t>, to_be: SeatStatus.t) => {
    s->Option.isSome && s->Option.getExn === to_be
  }

  let countSeat = (xs, seatStatus: SeatStatus.t) => {
    xs->Array.keep(isSeatEq(_, seatStatus))->Array.length
  }

  let countEmptySeat = countSeat(_, #L)
  let countFloor = countSeat(_, #".")
  let countOccupiedSeat = countSeat(_, #"#")

  let transform = (s: SeatStatus.t, adjacents) => {
    let occupied_seats = adjacents->countOccupiedSeat
    switch s {
    | #L => occupied_seats == 0 ? #"#" : #L

    | #"#" => occupied_seats >= 4 ? #L : #"#"
    | #"." => #"."
    }
  }

  let iterate = t => {
    t->Array2D.mapWithIndex(((x, y), s) => {
      s->transform(t->getAdjacents((x, y)))
    })
  }

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

  //  let adj = seats->SeatMap.getAdjacents((2, 0))
  //  adj->log
  //  adj->SeatMap.countEmptySeat->log
  "seats"->log
  seats->SeatMap.dump
  let iter_1 = seats->SeatMap.iterate
  "iter_1"->log
  iter_1->SeatMap.dump

  let iter_2 = iter_1->SeatMap.iterate
  "iter_2"->log
  iter_2->SeatMap.dump

  let iter_3 = iter_2->SeatMap.iterate
  "iter_3"->log
  iter_3->SeatMap.dump

  let iter_4 = iter_3->SeatMap.iterate
  "iter_4"->log
  iter_4->SeatMap.dump

  let iter_5 = iter_4->SeatMap.iterate
  "iter_5"->log
  iter_5->SeatMap.dump

  1
}

let solvePart2 = data => {
  data->ignore
  2
}