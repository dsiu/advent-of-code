open Stdlib

type t<'a> = array<array<'a>>

let make = ((x, y), e: 'a) => {
  Array.makeBy(y, _ => Array.make(~length=x, e))
}

let copy = t => {
  t->Array.map(Array.concat(_, []))
}

let lengthY = t => t->Array.length

let lengthX = t => {
  t->Array.get(0)->Stdlib.Option.mapOr(0, a => Array.length(a))
}

let isValidXY = (t, (x, y)) => {
  let len_x = t->lengthX
  let len_y = t->lengthY

  x >= 0 && x <= len_x - 1 && y >= 0 && y <= len_y - 1
}

let set = (t, (x, y), e: 'a) => {
  switch t->Array.get(y) {
  | Some(y) => y->Array.set(x, e)
  | None => ()
  }
}

let setYEquals = (t, y, e: array<'a>) => {
  t->Array.set(y, e)
}

let get = (t, (x, y)) => {
  switch t->Array.get(y) {
  | Some(y) => y->Array.get(x)
  | None => None
  }
}

let getExn = (t, (x, y)) => {
  t->Array.getUnsafe(y)->Array.getUnsafe(x)
}

let getYEquals = (t, y) => {
  t->Array.get(y)
}

let getXEquals = (t, x) => {
  x < t->lengthX
    ? {
        let ret = t->Array.reduce([], (a, xs) => {
          Array.concat(a, [xs->Array.getUnsafe(x)])
        })
        ret->Array.length === t->lengthY ? Some(ret) : None
      }
    : None
}

//let keep = (t, f) => {
//  t->Array.map(Array.keep(_, f))
//}

let map = (t, f) => {
  t->Array.map(x => x->Array.map(f))
}

let mapWithIndex = (t, f) => {
  t->Array.mapWithIndex((xs, j) => {
    xs->Array.mapWithIndex((e, i) => f((i, j), e))
  })
}

let reduce = (t, a, f) => {
  t->Array.reduce(a, (acc, x) => x->Array.reduce(acc, f))
}

let reduceWithIndex = (t, a, f) => {
  t->Array.reduceWithIndex(a, (acc, xs, yi) =>
    xs->Array.reduceWithIndex(acc, (acc, x, xi) => f(acc, x, (xi, yi)))
  )
}

let flatten = t => {
  let ret = ref([])
  for i in 0 to t->lengthY - 1 {
    ret := Array.concat(ret.contents, t->getYEquals(i)->Option.getWithDefault([]))
  }
  ret.contents
}

let crop = (t, (x, y), ~len_x, ~len_y) => {
  let sizeX = t->lengthX
  let sizeY = t->lengthY
  t->isValidXY((x, y))
    ? {
        let ret = ref([])
        let max_x_len = sizeX - x
        let adj_len_x = min(len_x, max_x_len)
        let max_y_len = sizeY - y
        let adj_y = min(y + len_y, y + max_y_len) - 1
        for i in y to adj_y {
          ret :=
            Array.concat(
              ret.contents,
              //              [t->getYEquals(i)->Option.getWithDefault([])->Array.slice(~offset=x, ~len=adj_len_x)],
              [
                t
                ->getYEquals(i)
                ->Option.getWithDefault([])
                ->Array.slice(~start=x, ~end=x + adj_len_x),
              ],
            )
        }
        ret.contents
      }
    : []
}

let eq = (t, u) => {
  t->lengthX === u->lengthX &&
  t->lengthY === u->lengthY &&
  Belt.Array.reduceReverse2(t, u, true, (c, a, b) => {
    c && Array.equal(a, b, (a, b) => {a === b})
  })
}

let toString = (t, f) => {
  let arrToStr = Utils.Printable.Array.toString
  t->Array.map(x => x->Array.map(f))->arrToStr(x => x->arrToStr(Stdlib.Function.identity) ++ "\n")
}
