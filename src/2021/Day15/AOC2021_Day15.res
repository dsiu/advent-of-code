open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

@@warning("-27")

open Graph

module Cave = {
  module E = {
    type t = int
    let compare = Pervasives.compare
    let default = 0
  }

  module V = {
    type t = (int, int)
  }

  module W = (
    E: {
      type t
      type label
      let label: t => label
    },
  ) => {
    type edge = E.t
    type t = int
    let weight = E.label
    let zero = 0
    let add = \"+"
    let compare = compare
  }

  //  module G = Graph.Imperative.Digraph.ConcreteLabeled(V, E)
  module G = Graph.Imperative.Digraph.AbstractLabeled(V, E)
  //  module G = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(V, E)

  module Dij = Path.Dijkstra(G, W(G.E))

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

  type t = {
    nodes: Array2D.t<G.V.t>,
    g: G.t,
  }

  let makeNodes = (g, lines) => {
    lines->Array2D.mapWithIndex((c, _) => {
      let v = G.V.create(c)
      G.add_vertex(g, v)
      v
    })
  }

  let node = (nodes, x, y) => Array2D.getExn(nodes, (x, y))

  let make = lines => {
    let g = G.create()
    let nodes = makeNodes(g, lines)

    let g = lines->Array2D.reduceWithIndex(g, (g, e, (x, y) as c) => {
      //      let h = V.hash((x, y))->Int.toString
      //      log(`${x->Int.toString},${y->Int.toString} (${h}) = ${e}`)->ignore
      let v = node(nodes, x, y)
      lines
      ->getAdjacents(c)
      ->List.forEach((CoordAndVal((x', y'), cost)) => {
        let v' = node(nodes, x', y')
        G.add_edge_e(g, G.E.create(v, cost, v'))
        //        log(
        //          `(${x->Int.toString}, ${y->Int.toString})[${V.hash((
        //              x,
        //              y,
        //            ))->Int.toString}], ${cost->Int.toString}, (${x'->Int.toString}, ${y'->Int.toString})[(${V.hash((
        //              x',
        //              y',
        //            ))->Int.toString})]`,
        //        )->ignore
      })

      //      G.add_vertex(g, (x, y))
      g
    })

    {nodes: nodes, g: g}
  }

  let solve = ({nodes, g}) => {
    let dest_x = nodes->Array2D.lengthX - 1
    let dest_y = nodes->Array2D.lengthY - 1
    let (path, w) = Dij.shortest_path(g, node(nodes, 0, 0), node(nodes, dest_x, dest_y))

    //    List.forEach(path, e => {
    //      Js.log4("src: ", e->G.E.src->G.V.label, " dst:", e->G.E.dst->G.V.label)
    //    })
    w
  }

  let expand = (map_orig, x_times, y_times) => {
    let sz_x_orig = map_orig->Array2D.lengthX
    let sz_y_orig = map_orig->Array2D.lengthY

    let sz_x_final = sz_x_orig * x_times
    let sz_y_final = sz_y_orig * y_times

    let map = Array2D.make((sz_x_final, sz_y_final), 0)

    for x_ep in 0 to x_times - 1 {
      for y_ep in 0 to y_times - 1 {
        map_orig
        ->Array2D.reduceWithIndex(0, (a, e, (x', y')) => {
          let x_new = x' + x_ep * sz_x_orig
          let y_new = y' + y_ep * sz_y_orig
          let e_new = {
            let e' = mod(e + x_ep + y_ep, 9)
            e' == 0 ? 9 : e'
          }
          //          Js.log2(x_new, y_new)
          map->Array2D.set((x_new, y_new), e_new)->ignore
          0
        })
        ->ignore
      }
    }

    map
  }

  let dump_graph = g => {
    Js.log("Vertex")
    G.iter_vertex(v => {
      Js.log2("vertex: ", v)
    }, g)
    Js.log("Edges")
    G.iter_edges_e(e => {
      Js.log4("src: ", e->G.E.src, " dst:", e->G.E.dst)
    }, g)
  }

  module Display = {
    include G
    let vertex_name = (v: V.t) => {
      //      G.V.label(v)
      //            let (x, y) = v
      let (x, y) = G.V.label(v)
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
  let output = ({nodes: _, g}) => {
    let () = Gv.fprint_graph(Format.str_formatter, g)
    Format.flush_str_formatter()
  }
}

let parse = data => {
  data
  ->splitNewline
  ->Array.map(l => {
    l->Js.String2.trim->splitChars->Array.map(c => c->Int.fromString->Option.getExn)
  })
}

let solvePart1 = data => {
  let cave = data->parse->Cave.make
  //  cave->Cave.output->Js.log
  cave->Cave.solve
  //  Cave.make_test_graph()->Cave.dump_graph->Js.log
  //  Cave.test_graph()->Js.log
}

let solvePart2 = data => {
  let new_cave = data->parse->Cave.expand(5, 5)
  //  new_cave->Js.log
  let cave = new_cave->Cave.make
  cave->Cave.solve
}
