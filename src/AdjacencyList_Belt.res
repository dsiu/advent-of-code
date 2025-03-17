module type S = {
  type v // vertex element type
  type e // edge element type
  type c // container type for edges
  type t // container type for vertexes

  let make: unit => t

  let addVertex: (t, v) => unit
  let removeVertex: (t, v) => bool
  let getVertex: (t, v) => array<e>
  let addEdge: (t, v, e) => unit
  let removeEdge: (t, v, e) => bool
  let adjacent: (t, v, e) => bool
  let neighbors: (t, v) => array<e>

  let toString: t => string
}

module type BASE = {
  type v
  type e
  type c = Set.t<e>
  type t = Map.t<v, c>

  let containerMake: unit => c
  let containerAdd: (c, e) => unit
  let containerRemove: (c, e) => bool
  let containerHas: (c, e) => bool
  let containerToArray: c => array<e>

  let containerToString: c => string
  let vertexToString: v => string
  let vertexFromString: string => v

  let make: unit => t
}

module Make = (BASE: BASE): (
  S with type e := BASE.e and type c := BASE.c and type v = BASE.v and type t = BASE.t
) => {
  // type e = BASE.e
  // type c = BASE.c
  type v = BASE.v
  type t = BASE.t

  let make = Map.make

  let containerMake = BASE.containerMake
  let containerAdd = BASE.containerAdd
  let containerRemove = BASE.containerRemove
  let containerHas = BASE.containerHas
  let containerToArray = BASE.containerToArray

  let vertexToString = BASE.vertexToString
  //  let vertexFromString = BASE.vertexFromString

  // return vertex's set
  let addVertex = (t, x) => {
    switch t->Map.get(x) {
    | Some(_) => ()
    | None => t->Map.set(x, containerMake())
    }
  }

  let removeVertex = (t, x) => {
    t->Map.delete(x)
  }

  let getVertex = (t, x) => {
    switch t->Map.get(x) {
    | Some(v) => v->containerToArray
    | None => raise(Not_found) // shouldn't really happen
    }
  }

  // will also add the vertex if it doesn't exist
  let addEdge = (t, x, e) => {
    t->addVertex(x)
    switch t->Map.get(x) {
    | Some(c) => c->containerAdd(e)
    | None => raise(Not_found) // shouldn't really happen
    }
  }

  let removeEdge = (t, x, y) => {
    switch t->Map.get(x) {
    | Some(v) => v->containerRemove(y)
    | None => false
    }
  }

  let adjacent = (t, x, y) => {
    switch t->Map.get(x) {
    | Some(v) => v->containerHas(y)
    | None => false
    }
  }

  let neighbors = (t, x) => {
    t->Map.get(x)->Option.getOr(containerMake())->containerToArray
  }

  let toString = t => {
    let str = ref("")
    t->Map.forEachWithKey((v, k) => {
      str :=
        `${str.contents}${k->vertexToString}: [ ${v->BASE.containerToArray->Array.toString} ]\n`
    })
    str.contents
  }
}

module BASE_Impl = {
  let make = Map.make

  let containerMake = Set.make
  let containerAdd = Set.add
  let containerRemove = Set.delete
  let containerHas = Set.has
  let containerToArray = Set.toArray

  let containerToString = s => s->Set.toArray->Array.toString
  let vertexToString = v => v
  let vertexFromString = v => v
}

module String = Make({
  type v = string
  type e = string
  type c = Set.t<e>
  type t = Map.t<v, c>

  include BASE_Impl
})

module TupleImpl = {
  module T = {
    type t = (string, int)
    let eq = ((s1, _): t, (s2, _): t) => s1 === s2
    let cmp = ((s1, _): t, (s2, _): t) => compare(s1, s2)

    type seed = int

    // from belt_MutableSetString.ml
    @val external caml_hash_mix_string: (seed, string) => seed = "caml_hash_mix_string"
    @val external final_mix: seed => seed = "caml_hash_final_mix"

    let hash = ((s, _): t) => final_mix(caml_hash_mix_string(0, s))
  }

  module MutableSetTuple = Belt.Id.MakeComparable(T)
}

module Tuple_Old = Make({
  module T = TupleImpl.T
  module MutableSetTuple = TupleImpl.MutableSetTuple

  type e = T.t
  type c = Set.t<MutableSetTuple.t>
  type v = string

  let containerMake = Set.make
  let containerAdd = Set.add
  let containerRemove = Set.delete
  let containerHas = Set.has
  let containerToArray = Set.toArray

  let containerToString = s => s->Set.toArray->Array.toString
  let vertexToString = v => v
  let vertexFromString = v => v

  type t = Map.t<string, c>
  let make = Map.make
})

module Tuple = Make({
  type e = (string, int)
  type c = Set.t<e>
  type v = string
  type t = Map.t<v, c>

  include BASE_Impl
})
