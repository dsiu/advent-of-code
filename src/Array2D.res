open Belt

type t<'a> = array<array<'a>>

let make = ((x, y), e: 'a) => {
  Array.makeBy(y, _ => Array.make(x, e))
}

let lengthY = t => t->Array.length

let lengthX = t => t->Array.getExn(0)->Array.length

let isValidXY = (t, (x, y)) => {
  let len_x = t->lengthX
  let len_y = t->lengthY

  x >= 0 && x <= len_x - 1 && y >= 0 && y <= len_y - 1
}

let set = (t, (x, y), e: 'a) => {
  switch t->Array.get(y) {
  | Some(y) => y->Array.set(x, e)
  | None => false
  }
}

let get = (t, (x, y)) => {
  switch t->Array.get(y) {
  | Some(y) => y->Array.get(x)
  | None => None
  }
}

let getExn = (t, (x, y)) => {
  t->Array.getExn(y)->Array.getExn(x)
}

let getYEquals = (t, y) => {
  t->Array.get(y)
}

let getXEquals = (t, x) => {
  let ret = t->Array.reduce([], (a, xs) => {
    Array.concat(a, [xs->Array.getExn(x)])
  })
  ret->Array.length === t->lengthY ? Some(ret) : None
}

//let keep = (t, f) => {
//  t->Array.map(Array.keep(_, f))
//}

let map = (t, f) => {
  t->Array.map(x => x->Array.map(f))
}

let mapWithIndex = (t, f) => {
  t->Array.mapWithIndex((j, xs) => {
    xs->Array.mapWithIndex((i, e) => f((i, j), e))
  })
}

let flatten = t => {
  let ret = ref([])
  for i in 0 to t->lengthY - 1 {
    ret := Array.concat(ret.contents, t->getYEquals(i)->Option.getWithDefault([]))
  }
  ret.contents
}

let crop = (t, (x, y), ~len_x, ~len_y) => {
  let ret = ref([])
  for i in y to y + len_y - 1 {
    ret :=
      Array.concat(
        ret.contents,
        [t->getYEquals(i)->Option.getWithDefault([])->Array.slice(~offset=x, ~len=len_x)],
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
