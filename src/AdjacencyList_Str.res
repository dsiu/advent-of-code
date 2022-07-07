open Belt

module type S = {
  type e // edge element type
  type c // container type for edges
  type t // container type for vertexes
  let make: (~hintSize: int) => t

  let addVertex: (t, string) => unit
  let removeVertex: (t, string) => unit
  let getVertex: (t, string) => c
  let addEdge: (t, string, e) => unit
  let removeEdge: (t, string, e) => unit
  let adjacent: (t, string, e) => bool
  let neighbors: (t, string) => c

  let toString: t => string
}

module AdjList_String: S
  with type e = string
  and type c = HashSet.String.t
  and type t = HashMap.String.t<HashSet.String.t> = {
  type e = string
  type c = HashSet.String.t
  type t = HashMap.String.t<c>

  let make = HashMap.String.make

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
    switch t->HashMap.String.get(x) {
    | Some(v) => v->HashSet.String.has(y)
    | None => false
    }
  }

  let neighbors = (t, x) => {
    t->HashMap.String.get(x)->Option.getWithDefault(HashSet.String.make(~hintSize=40))
  }

  let toString = t => {
    let str = ref("")
    t->HashMap.String.forEach((k, v) => {
      str := `${str.contents}${k}: [ ${v->HashSet.String.toArray->Js.Array2.joinWith(_, ",")} ]\n`
    })
    str.contents
  }
}
