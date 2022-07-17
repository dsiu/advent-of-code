open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

open Graph

module Cave = {
  module E = {
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = \"="
    let default = 0
  }

  module V = {
    type t = (int, int)
    let compare = ((x1, y1), (x2, y2)) => {
      let dy = y2 - y1
      let dx = x2 - x1
      dx == 0 && dy == 0 ? 0 : dx > 0 && dy > 0 ? 1 : dx + dy
    }
    let equal = (v1, v2) => {compare(v1, v2) == 0}
    let seed = 31
    @val external caml_hash_mix_int: (int, int) => int = "caml_hash_mix_int"
    @val external final_mix: int => int = "caml_hash_final_mix"

    let hash_int = x => final_mix(caml_hash_mix_int(seed, x))
    let hash = ((x, y)) => hash_int(x) + seed * hash_int(y)
  }

  module G = Graph.Imperative.Graph.ConcreteLabeled(V, E)

  let make = lines => {
    lines
    ->Array2D.reduceWithIndex("", (a, e, (x, y)) => {
      let h = V.hash((x, y))->Int.toString

      log(`${x->Int.toString},${y->Int.toString} (${h}) = ${e}`)
      ""
    })
    ->ignore
    ""
  }
}

let parse = data => {
  let lines =
    data
    ->splitNewline
    ->Array.map(l => {
      l->Js.String2.trim->splitChars
    })
  let y = lines->Array.size

  y->log2("y")
  let x = lines[0]->Option.flatMap(x => Some(x->Array.size))->Option.getExn
  x->log2("x")

  lines
}

let solvePart1 = data => {
  let cave = data->parse->Cave.make
  //  cave
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
