open Belt

type t<'a> = array<array<'a>>

let make = ((x, y), e: 'a) => {
  Array.makeBy(x, _ => Array.make(y, e))
}

let lengthX = t => t->Array.length

let lengthY = t => t->Array.getExn(0)->Array.length

let isValidXY = (t, (x, y)) => {
  let len_x = t->lengthX
  let len_y = t->lengthY

  x >= 0 && x <= len_x - 1 && y >= 0 && y <= len_y - 1
}

let set = (t, (x, y), e: 'a) => {
  switch t->Array.get(x) {
  | Some(x) => x->Array.set(y, e)
  | None => false
  }
}

let get = (t, (x, y)) => {
  switch t->Array.get(x) {
  | Some(x) => x->Array.get(y)
  | None => None
  }
}

let getExn = (t, (x, y)) => {
  t->Array.getExn(x)->Array.getExn(y)
}

let getXEquals = (t, x) => {
  t->Array.get(x)
}

let getYEquals = (t, y) => {
  let ret = t->Array.reduce([], (a, ys) => {
    Array.concat(a, [ys->Array.getExn(y)])
  })
  ret->Array.length === t->lengthX ? Some(ret) : None
}

//let keep = (t, f) => {
//  t->Array.map(Array.keep(_, f))
//}

let map = (t, f) => {
  t->Array.map(x => x->Array.map(f))
}

let mapWithIndex = (t, f) => {
  t->Array.mapWithIndex((i, ys) => {
    ys->Array.mapWithIndex((j, e) => f((i, j), e))
  })
}

let flatten = t => {
  let ret = ref([])
  for i in 0 to t->lengthX - 1 {
    ret := Array.concat(ret.contents, t->getXEquals(i)->Option.getWithDefault([]))
  }
  ret.contents
}

let crop = (t, (x, y), ~len_x, ~len_y) => {
  let ret = ref([])
  for i in x to x + len_x - 1 {
    ret :=
      Array.concat(
        ret.contents,
        [t->getXEquals(i)->Option.getWithDefault([])->Array.slice(~offset=y, ~len=len_y)],
      )
  }
  ret.contents
}

let eq = (t, u) => {
  t->lengthX === u->lengthX &&
  t->lengthY === u->lengthY &&
  Array.reduceReverse2(t, u, true, (c, a, b) => {
    c && Array.eq(a, b, (a, b) => {a === b})
  })
}
