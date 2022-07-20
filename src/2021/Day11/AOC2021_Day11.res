open Belt
open Utils
let log = Js.Console.log

module Octopus = {
  type t = Array2D.t<int>

  let adjCoords = c => {
    open Coordinate
    [stepNW, stepN, stepNE, stepW, stepE, stepSW, stepS, stepSE]->Array.map(f => f(. c))
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
    ->Array.keepMapU((. c) => {
      t->Array2D.isValidXY(c) ? Some(t->Array2D.getExn(c)) : None
    })
  }

  let count9Plus = t => t->Array.keep(_, b => b >= 9)->Array.size

  let countZero = t => t->Array.keep(_, b => b == 0)->Array.size

  let increaseEnergy = t => t->Array2D.mapU((. x) => add(1, x))

  let getFlashingCoords = t => {
    t->Array2D.reduceWithIndex([], (a, e, coord) => {
      switch e > 9 {
      | true => Array.concat(a, [coord])
      | false => a
      }
    })
  }

  // will modify t in place
  let performFlash = (t, coord) => {
    let neighbors = getAdjacentCoords(t, coord)

    neighbors->Array.forEachU((. n_addr) => {
      let orig = Array2D.getExn(t, n_addr)
      switch Array2D.set(t, n_addr, {orig > 0 ? orig + 1 : orig}) {
      | true => ()
      | false => raise(Not_found)
      }
    })

    t
  }

  // will modify t in place
  let dim = (t, coord) => {
    switch t->Array2D.set(coord, 0) {
    | true => t
    | false => raise(Not_found)
    }
  }

  let iterate = t => {
    let next = t->increaseEnergy

    let rec inner = t => {
      let flashings = t->getFlashingCoords

      switch flashings->Array.size > 0 {
      | true => {
          flashings->Array.forEachU((. flash_coord) => {
            t->performFlash(flash_coord)->dim(flash_coord)->ignore
          })
          inner(t)
        }
      | false => t
      }
    }
    inner(next)
  }

  type collector<'a, 'b> = ('a, 'b) => 'a

  let rec iterateAndReduceN = (t, n, acc, reducer: collector<'a, 'b>) => {
    let next = t->iterate
    let acc = reducer(acc, t)

    n - 1 < 0 ? acc : iterateAndReduceN(next, n - 1, acc, reducer)
  }

  // accumulate all flashes
  let countFlashN = (t, n) => {
    iterateAndReduceN(t, n, 0, (acc, t) => {
      acc + t->Array2D.flatten->countZero
    })
  }

  // result of N iteration
  let iterateN = (t, n) => {
    iterateAndReduceN(t, n, t, (_, t) => t)
  }

  // number of flashes at N step
  let flashesAtN = (t, n) => {
    iterateAndReduceN(t, n, 0, (_, t) => {
      t->Array2D.flatten->countZero
    })
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

let solvePart1_try = data => {
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
}

let solvePart1 = data => {
  let d = data->parse
  let i = 100

  d->Octopus.countFlashN(_, i)
}

let solvePart2 = data => {
  let d = data->parse

  //  d->Octopus.iterateN(_, 195)->Octopus.toString->Js.log
  //  d->Octopus.flashesAtN(_, 195)->Js.log

  let i = ref(0)
  let c = ref(0)

  while c.contents < 100 {
    //    Js.log(`iterate ${i.contents->Int.toString} ----`)
    c := d->Octopus.flashesAtN(_, i.contents)
    i := i.contents + 1
  }

  i.contents - 1
}
