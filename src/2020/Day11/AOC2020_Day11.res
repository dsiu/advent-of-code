open Belt
let log = Js.Console.log
//open Utils

module SeatMap = {
  exception InvalidStatus(string)

  module SeatStatus = {
    // L = Empty
    // # = Occupied
    // . = Floor
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

  let adjCoords = (. c) => {
    open Coordinate
    [stepNW, stepN, stepNE, stepW, stepE, stepSW, stepS, stepSE]->Array.mapU((. f) => f(. c))
  }

  let getAdjacents = (t, (x, y)) => {
    adjCoords(. (x, y))->Array.keepMap(c => {
      t->Array2D.isValidXY(c) ? Some(t->Array2D.getExn(c)) : None
    })
  }

  let isSeatEq = (s: SeatStatus.t, to_be: SeatStatus.t) => {
    s === to_be
  }

  let countSeat = (xs, seatStatus: SeatStatus.t) => {
    xs->Array.keep(isSeatEq(_, seatStatus))->Array.length
  }

  let countEmptySeat = countSeat(_, #L)
  let countFloor = countSeat(_, #".")
  let countOccupiedSeat = countSeat(_, #"#")

  let transformPart1 = (s: SeatStatus.t, adjacents) => {
    let occupied_seats = adjacents->countOccupiedSeat
    switch s {
    | #L => occupied_seats == 0 ? #"#" : #L

    | #"#" => occupied_seats >= 4 ? #L : #"#"
    | #"." => #"."
    }
  }

  let iteratePart1 = t => {
    t->Array2D.mapWithIndexU((. (x, y), s) => s->transformPart1(t->getAdjacents((x, y))))
  }

  // part 2
  let rec nextSeatIn = (. t, (x, y), step) => {
    let c = step(. (x, y))
    t->Array2D.isValidXY(c)
      ? {
          switch t->Array2D.get(c)->Option.getExn {
          | #"." => nextSeatIn(. t, c, step)
          | seat => seat
          }
        }
      : #"."
  }

  let getDirectionals = (t, c) => {
    open Coordinate
    [stepNW, stepN, stepNE, stepW, stepE, stepSW, stepS, stepSE]->Array.mapU((. f) =>
      nextSeatIn(. t, c, f)
    )
  }

  let transformPart2 = (s: SeatStatus.t, directionals) => {
    let occupied_seats = directionals->countOccupiedSeat
    switch s {
    | #L => occupied_seats == 0 ? #"#" : #L
    | #"#" => occupied_seats >= 5 ? #L : #"#"
    | #"." => #"."
    }
  }

  let iteratePart2 = t => {
    t->Array2D.mapWithIndexU((. (x, y), s) => s->transformPart2(t->getDirectionals((x, y))))
  }

  let isStabilized = Array2D.eq

  let rec stabilize = (t, solver): t => {
    let t_next = t->solver
    isStabilized(t, t_next) ? t : t_next->stabilize(solver)
  }

  let solvePart1 = stabilize(_, iteratePart1)
  let solvePart2 = stabilize(_, iteratePart2)

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
  let result = seats->SeatMap.solvePart1
  let result_flat = result->Array2D.flatten
  //  result->SeatMap.dump
  result_flat->SeatMap.countOccupiedSeat
}

let solvePart2 = data => {
  let seats = data->parse
  let result = seats->SeatMap.solvePart2
  let result_flat = result->Array2D.flatten
  //  result->SeatMap.dump
  result_flat->SeatMap.countOccupiedSeat
}
