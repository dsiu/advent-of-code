open Belt

type t<'a> = array<array<'a>>

let make = ((x, y), e: 'a) => {
  Array.makeBy(x, _ => Array.make(y, e))
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

let keep = (t, f) => {
  t->Array.map(Array.keep(_, f))
}

let map = (t, f) => {
  t->Array.map(x => x->Array.map(f))
}
