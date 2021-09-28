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

    (x, y)->adjCoords->Array.keep(isValidCoord(_, ~len_x, ~len_y))->Array.map(t->Array2D.getExn)
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

  let north = ((x, y)) => {(x, y - 1)}
  let east = ((x, y)) => {(x + 1, y)}
  let south = ((x, y)) => {(x, y + 1)}
  let west = ((x, y)) => {(x - 1, y)}
  let northEast = c => c->north->east
  let northWest = c => c->north->west
  let southEast = c => c->south->east
  let southWest = c => c->south->west

  let stepFunc = ((x, y), f) => (x, y)->f

  let stepN = stepFunc(_, north)
  let stepE = stepFunc(_, east)
  let stepS = stepFunc(_, south)
  let stepW = stepFunc(_, west)
  let stepNE = stepFunc(_, northEast)
  let stepNW = stepFunc(_, northWest)
  let stepSE = stepFunc(_, southEast)
  let stepSW = stepFunc(_, southWest)

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

  let isStabilized = Array2D.eq

  let rec stabilize = (t): t => {
    let t_next = t->iterate
    isStabilized(t, t_next) ? t : t_next->stabilize
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
  let result = seats->SeatMap.stabilize
  let result_flat = result->Array2D.flatten
  //  result->SeatMap.dump
  result_flat->SeatMap.countOccupiedSeat
}

let solvePart2 = data => {
  data->ignore
  2
}
