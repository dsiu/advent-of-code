//
// mutable graph
//

open Belt

type t = HashMap.String.t<HashSet.String.t>

let make = HashMap.String.make

let toString = t => {
  let str = ref("")
  t->HashMap.String.forEach((k, v) => {
    str := `${str.contents}${k}: [ ${v->HashSet.String.toArray->Js.Array2.joinWith(_, ",")} ]\n`
  })
  str.contents
}

// return vertex's set
let addVertex = (t, x) => {
  switch t->HashMap.String.get(x) {
  | Some(_) => ()
  | None => t->HashMap.String.set(x, HashSet.String.make(~hintSize=40))
  }
}

let removeVertex = (t, x) => {
  t->HashMap.String.remove(x)
}

// will create vertex if not exist
let getVertex = (t, x) => {
  t->addVertex(x)
  switch t->HashMap.String.get(x) {
  | Some(v) => v
  | None => raise(Not_found) // shouldn't really happen
  }
}

let addEdge = (t, x, y) => {
  t->getVertex(x)->HashSet.String.add(y)
}

let removeEdge = (t, x, y) => {
  switch t->HashMap.String.has(x) {
  | true => t->getVertex(x)->HashSet.String.remove(y)
  | false => ()
  }
}

let adjacent = (t, x, y) => {
  switch t->HashMap.get(x) {
  | Some(v) => v->HashSet.String.has(y)
  | None => false
  }
}

let neighbors = (t, x) => {
  switch t->HashMap.get(x) {
  | Some(v) => v->HashSet.String.toArray
  | None => []
  }
}
