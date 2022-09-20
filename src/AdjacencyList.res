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

module type BASE = {
  type e

  type c
  let containerMake: (~hintSize: int) => c
  let containerAdd: (c, e) => unit
  let containerRemove: (c, e) => unit
  let containerHas: (c, e) => bool
  let containerToArray: c => array<e>

  type t = HashMap.String.t<c>
  let make: (~hintSize: int) => t
}

module Make = (BASE: BASE): (S with type e := BASE.e and type c := BASE.c and type t = BASE.t) => {
  //  type e = BASE.e
  //  type c = BASE.c
  type t = BASE.t
  let make = HashMap.String.make
  let containerMake = BASE.containerMake
  let containerAdd = BASE.containerAdd
  let containerRemove = BASE.containerRemove
  let containerHas = BASE.containerHas
  let containerToArray = BASE.containerToArray

  // return vertex's set
  let addVertex = (t, x) => {
    switch t->HashMap.String.get(x) {
    | Some(_) => ()
    | None => t->HashMap.String.set(x, containerMake(~hintSize=40))
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

  let addEdge = (t, x, e) => {
    t->getVertex(x)->containerAdd(e)
  }

  let removeEdge = (t, x, y) => {
    switch t->HashMap.String.has(x) {
    | true => t->getVertex(x)->containerRemove(y)
    | false => ()
    }
  }

  let adjacent = (t, x, y) => {
    switch t->HashMap.String.get(x) {
    | Some(v) => v->containerHas(y)
    | None => false
    }
  }

  let neighbors = (t, x) => {
    t->HashMap.String.get(x)->Option.getWithDefault(containerMake(~hintSize=40))
  }

  let toString = t => {
    let str = ref("")
    t->HashMap.String.forEachU((. k, v) => {
      str := `${str.contents}${k}: [ ${v->containerToArray->Js.Array2.joinWith(_, ",")} ]\n`
    })
    str.contents
  }
}

module String = Make({
  type e = string
  type c = HashSet.String.t
  let containerMake = HashSet.String.make
  let containerAdd = HashSet.String.add
  let containerRemove = HashSet.String.remove
  let containerHas = HashSet.String.has
  let containerToArray = HashSet.String.toArray

  type t = HashMap.String.t<HashSet.String.t>
  let make = HashMap.String.make
})

module TupleImpl = {
  module T = {
    type t = (string, int)
    let eq = ((s1, _): t, (s2, _): t) => s1 === s2

    type seed = int

    // from belt_HashSetString.ml
    @val external caml_hash_mix_string: (seed, string) => seed = "caml_hash_mix_string"
    @val external final_mix: seed => seed = "caml_hash_final_mix"

    let hash = ((s, _): t) => final_mix(caml_hash_mix_string(0, s))
  }

  module HashSetTuple = Belt.Id.MakeHashable(T)
}

module Tuple = Make({
  module T = TupleImpl.T
  module HashSetTuple = TupleImpl.HashSetTuple

  type e = T.t
  type c = HashSet.t<HashSetTuple.t, HashSetTuple.identity>

  let containerMake = (~hintSize) => HashSet.make(~hintSize, ~id=module(HashSetTuple))
  let containerAdd = HashSet.add
  let containerRemove = HashSet.remove
  let containerHas = HashSet.has
  let containerToArray = HashSet.toArray

  type t = HashMap.String.t<HashSet.t<HashSetTuple.t, HashSetTuple.identity>>
  let make = HashMap.String.make
})
