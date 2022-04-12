open Belt
open Utils
let log = Js.Console.log

module Octopus = {
  type t = Array2D.t<int>

  let adjCoords = c => {
    open Coordinate
    [stepNW, stepN, stepNE, stepW, stepE, stepSW, stepS, stepSE]->Array.map(f => c->f)
  }

  let getAdjacentCoords = (t, c) => {
    c
    ->adjCoords
    ->Array.keepMap(c => {
      t->Array2D.isValidXY(c) ? Some(c) : None
    })
  }

  let getAdjacents = (t, (x, y)) => {
    (x, y)
    ->adjCoords
    ->Array.keepMap(c => {
      t->Array2D.isValidXY(c) ? Some(t->Array2D.getExn(c)) : None
    })
  }

  let count9Plus = t => t->Array.keep(_, b => b >= 9)->Array.size
  let increaseEnergy = t => t->Array2D.map(b => b + 1)

  let iterate = t => {
    let next = t->increaseEnergy

    let rec inner = t => {
      let flashing_octopus = t->Array2D.reduceWithIndex([], (a, e, coord) => {
        switch e > 9 {
        | true => Array.concat(a, [coord])
        | false => a
        }
      })

      switch flashing_octopus->Array.size > 0 {
      | true => {
          flashing_octopus->Array.forEach(flash_coord => {
            // flash on neighbors
            let coords = getAdjacentCoords(t, flash_coord)

            coords->Array.forEach(coord => {
              let orig = Array2D.getExn(t, coord)
              Array2D.set(t, coord, {orig > 0 ? orig + 1 : orig})->ignore
            })
            // dim itself
            t->Array2D.set(flash_coord, 0)->ignore
          })

          inner(t)
        }
      | false => t
      }
    }

    inner(next)
  }

  let rec iterateN = (t, n) => {
    let next = t->iterate
    n - 1 < 0 ? t : iterateN(next, n - 1)
  }

  let toString = t => {
    let ret = ref(([]: array<string>))
    for i in 0 to t->Array2D.lengthY - 1 {
      let row = t->Array2D.getYEquals(i)->Option.getWithDefault([])
      ret :=
        Array.concat(
          ret.contents,
          [row->Array.map(x => x->Js.Int.toString)->Js.Array2.joinWith(_, "")],
        )
    }
    ret.contents->Js.Array2.joinWith(_, "\n")
  }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(
    FP_Utils.compose(Js.String2.trim, x => x->Utils.splitChars->Array.map(intFromStringExn)),
  )

let solvePart1 = data => {
  Js.log("orig ----")
  let d = data->parse
  d->Octopus.toString->Js.log
  Js.log("orig ----")

  let i = 1
  Js.log(`iterate ${i->Int.toString} ----`)

  let e = d->Octopus.iterateN(_, i)
  e->Octopus.toString->Js.log
  Js.log(`iterate ${i->Int.toString} ----`)

  let i = 100
  Js.log(`iterate ${i->Int.toString} ----`)

  let e = d->Octopus.iterateN(_, i)
  e->Octopus.toString->Js.log
  Js.log(`iterate ${i->Int.toString} ----`)

  1
}

let solvePart2 = data => {
  data->ignore
  2
}
