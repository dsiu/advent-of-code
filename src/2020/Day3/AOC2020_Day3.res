open Belt
let log = Js.Console.log

module TreeMap = {
  type t

  let width = t => t[0]->Option.getExn->Array.length

  let height = t => t->Array.length

  let get = (t, (x, y)) => t->Array2D.get((x, y))

  let getWrapped = (t, (x, y)) => {
    let w = t->width
    let h = t->height
    let newX = mod(x, w)
    let newY = mod(y, h)
    //    let itos = Int.toString
    //    `w=${w->itos} x=${x->itos} newX=${newX->itos} | h=${h->itos} y=${y->itos} newY=${newY->itos}`->log
    t->Array2D.get((newX, newY))
  }

  let isTree = c => {c == "#"}

  let make = (xs: array<string>) => {
    let x = xs->Array.getExn(0)->Js.String2.length
    let y = xs->Array.length
    let ret = Array2D.make((x, y), "")

    xs->Array.forEachWithIndex((j, s) => {
      let ss = s->Js.String2.split("")
      ss->Array.forEachWithIndex((i, c) => {
        let _ = ret->Array2D.set((i, j), c)
      })
    })

    ret
  }

  let walk = (t, (side, down)) => {
    let h = t->height

    let rec walkAux = (count, (x, y)) => {
      let nextX = x + side
      let nextY = y + down
      let cur = t->getWrapped((x, y))->Option.getExn->isTree
      let done = y >= h

      switch (done, cur) {
      | (false, true) => walkAux(count + 1, (nextX, nextY))
      | (false, false) => walkAux(count, (nextX, nextY))
      | (true, _) => count
      }
    }

    walkAux(0, (0, 0))
  }
}

let solvePart1 = data => {
  let parsed = data->Js.String2.split("\n")->Array.map(Js.String2.trim)
  TreeMap.make(parsed)->TreeMap.walk(_, (3, 1))
}

let solvePart2 = data => {
  let parsed = data->Js.String2.split("\n")->Array.map(Js.String2.trim)
  let tm = TreeMap.make(parsed)
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let trees = slopes->Array.map(s => tm->TreeMap.walk(_, s))
  //  trees->log

  open RescriptCore
  let acc = BigInt.fromInt(1)
  trees
  ->Belt.Array.reduce(acc, (a, s) => {
    let ss = BigInt.fromInt(s)
    let ret = BigInt.mul(a, ss)

    //    let itos = Js.Int.toString
    //    let bitos = BigInt.toString

    //    `a=${a->bitos} s=${s->itos} a*s=${ret->bitos}`->log
    ret
  })
  ->BigInt.toString
}
