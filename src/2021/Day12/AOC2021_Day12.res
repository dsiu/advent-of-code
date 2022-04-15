open Belt
open Utils
let log = Js.Console.log

module Graph = {
  type t = HashMap.String.t<HashSet.String.t>

  let make = HashMap.String.make

  // return vertex's set
  let addVertex = (t, x) => {
    switch t->HashMap.String.get(x) {
    | Some(_) => ()
    | None => t->HashMap.String.set(x, HashSet.String.make(~hintSize=40))
    }
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
}

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  data->ignore
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
