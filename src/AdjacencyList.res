open Belt

module type S = {
  type e // edge element type
  type c // container type for edges
  type t // container type for vertexes

  let make: unit => t

  let addVertex: (t, string) => unit
  let removeVertex: (t, string) => unit
  let getVertex: (t, string) => c
  let addEdge: (t, string, e) => unit
  let removeEdge: (t, string, e) => unit
  let adjacent: (t, string, e) => bool
  let neighbors: (t, string) => c

  let toString: t => string
}

module type BASE = {
  type e

  type c
  let containerMake: unit => c
  let containerAdd: (c, e) => unit
  let containerRemove: (c, e) => unit
  let containerHas: (c, e) => bool
  let containerToArray: c => array<e>

  type t = MutableMap.String.t<c>
  let make: unit => t
}

module Make = (BASE: BASE): (S with type e := BASE.e and type c := BASE.c and type t = BASE.t) => {
  //  type e = BASE.e
  //  type c = BASE.c
  type t = BASE.t
  let make = MutableMap.String.make
  let containerMake = BASE.containerMake
  let containerAdd = BASE.containerAdd
  let containerRemove = BASE.containerRemove
  let containerHas = BASE.containerHas
  let containerToArray = BASE.containerToArray

  // return vertex's set
  let addVertex = (t, x) => {
    switch t->MutableMap.String.get(x) {
    | Some(_) => ()
    | None => t->MutableMap.String.set(x, containerMake())
    }
  }

  let removeVertex = (t, x) => {
    t->MutableMap.String.remove(x)
  }

  // will create vertex if not exist
  let getVertex = (t, x) => {
    t->addVertex(x)
    switch t->MutableMap.String.get(x) {
    | Some(v) => v
    | None => raise(Not_found) // shouldn't really happen
    }
  }

  let addEdge = (t, x, e) => {
    t->getVertex(x)->containerAdd(e)
  }

  let removeEdge = (t, x, y) => {
    switch t->MutableMap.String.has(x) {
    | true => t->getVertex(x)->containerRemove(y)
    | false => ()
    }
  }

  let adjacent = (t, x, y) => {
    switch t->MutableMap.String.get(x) {
    | Some(v) => v->containerHas(y)
    | None => false
    }
  }

  let neighbors = (t, x) => {
    t->MutableMap.String.get(x)->Option.getWithDefault(containerMake())
  }

  let toString = t => {
    let str = ref("")
    t->MutableMap.String.forEachU((. k, v) => {
      str := `${str.contents}${k}: [ ${v->containerToArray->Js.Array2.joinWith(_, ",")} ]\n`
    })
    str.contents
  }
}

module String = Make({
  type e = string
  type c = MutableSet.String.t
  let containerMake = MutableSet.String.make
  let containerAdd = MutableSet.String.add
  let containerRemove = MutableSet.String.remove
  let containerHas = MutableSet.String.has
  let containerToArray = MutableSet.String.toArray

  type t = MutableMap.String.t<MutableSet.String.t>
  let make = MutableMap.String.make
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

module Tuple = Make({
  module T = TupleImpl.T
  module MutableSetTuple = TupleImpl.MutableSetTuple

  type e = T.t
  type c = MutableSet.t<MutableSetTuple.t, MutableSetTuple.identity>

  let containerMake = () => MutableSet.make(~id=module(MutableSetTuple))
  let containerAdd = MutableSet.add
  let containerRemove = MutableSet.remove
  let containerHas = MutableSet.has
  let containerToArray = MutableSet.toArray

  type t = MutableMap.String.t<MutableSet.t<MutableSetTuple.t, MutableSetTuple.identity>>
  let make = MutableMap.String.make
})
