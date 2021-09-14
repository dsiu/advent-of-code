open Belt
let log = Js.Console.log
//open Utils

module SeatMap = {
  type t

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

  let getSurrendings = (t, (x, y)) => {
    [-1, 0, 1]
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
  data->parse->SeatMap.dump
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
