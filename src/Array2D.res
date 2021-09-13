open Belt

type t<'a> = array<array<'a>>

let make = ((x, y), e: 'a) => {
  Array.makeBy(x, _ => Array.make(y, e))
}

let lengthX = t => t->Array.length

let lengthY = t => t->Array.getExn(0)->Array.length

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
