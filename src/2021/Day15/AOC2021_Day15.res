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
    let hash = ((x, y)) => Hashtbl.hash((x, y))
  }

  module G = Graph.Imperative.Graph.ConcreteLabeled(V, E)

  let adjCoords = c => {
    open Coordinate
    list{stepN, stepW, stepE, stepS}->List.map(f => c->f)
  }

  type elem = CoordAndVal(Coordinate.t, int)

  let getAdjacents = (t, (x, y)) => {
    (x, y)
    ->adjCoords
    ->List.keepMap(c => {
      t->Array2D.isValidXY(c) ? Some(CoordAndVal(c, t->Array2D.getExn(c))) : None
    })
  }

  let make = lines => {
    lines->Array2D.reduceWithIndex(G.create(), (g, e, (x, y) as c) => {
      //      let h = V.hash((x, y))->Int.toString
      //      log(`${x->Int.toString},${y->Int.toString} (${h}) = ${e}`)->ignore
      lines
      ->getAdjacents(c)
      ->List.forEach((CoordAndVal((x', y'), cost)) => {
        G.add_edge_e(g, ((x, y), cost, (x', y')))
        log(
          `(${x->Int.toString}, ${y->Int.toString})[${V.hash((
              x,
              y,
            ))->Int.toString}], ${cost->Int.toString}, (${x'->Int.toString}, ${y'->Int.toString})[(${V.hash((
              x',
              y',
            ))->Int.toString})]`,
        )->ignore
      })

      //      G.add_vertex(g, (x, y))
      g
    })
  }

  module Display = {
    open Graphviz
    include G
    let vertex_name = v => {
      let (x, y) = V.label(v)
      `"${x->Int.toString},${y->Int.toString}"`
    }
    let graph_attributes = _ => list{}
    let default_vertex_attributes = _ => list{}
    let vertex_attributes = _ => list{}
    let default_edge_attributes = _ => list{}
    let edge_attributes = e => list{#Label(Int.toString(E.label(e)))}
    let get_subgraph = _ => None
  }

  module Gv = Graphviz.Dot(Display)

  @@warning("-3")
  let output = g => {
    let () = Gv.fprint_graph(Format.str_formatter, g)
    Format.flush_str_formatter()
  }
}

let parse = data => {
  let lines =
    data
    ->splitNewline
    ->Array.map(l => {
      l->Js.String2.trim->splitChars->Array.map(c => c->Int.fromString->Option.getExn)
    })

  //  let y = lines->Array.size
  //  let x = lines[0]->Option.flatMap(x => Some(x->Array.size))->Option.getExn
  lines
}

let solvePart1 = data => {
  let cave = data->parse->Cave.make
  cave->Cave.output->Js.log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
