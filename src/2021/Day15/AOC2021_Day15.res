@@uncurried

open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

open Graphology

module Cave = {
  let adjCoords = c => {
    open Coordinate.StepFunctions
    list{stepN, stepW, stepE, stepS}->List.map(f => f(c))
  }

  type elem = CoordAndVal(Coordinate.t, int)

  let getAdjacents = (t, (x, y)) => {
    (x, y)
    ->adjCoords
    ->List.keepMapU(c => {
      t->Array2D.isValidXY(c) ? Some(CoordAndVal(c, t->Array2D.getExn(c))) : None
    })
  }

  module G = Graph.MakeGraph({
    type node = (int, int)
    type edge = int
  })

  /* * type of Cave */
  type t = {
    nodes: Array2D.t<(int, int)>,
    g: G.t,
  }

  /* * create all the vertex and keep them in a Array2D */
  let makeNodes = (g, lines) => {
    lines->Array2D.mapWithIndex((c, _) => {
      //      let v = G.V.create(c)
      g->G.addNode(c)
      c
    })
  }

  /* * get the vertex from Array2D */
  let node = (nodes, x, y) => Array2D.getExn(nodes, (x, y))

  let make = lines => {
    let g = G.makeDirectedGraph()
    let nodes = makeNodes(g, lines)

    let g = lines->Array2D.reduceWithIndex(g, (g, _e, (x, y) as c) => {
      //      let h = V.hash((x, y))->Int.toString
      //      log(`${x->Int.toString},${y->Int.toString} (${h}) = ${e}`)->ignore
      let v = node(nodes, x, y)
      lines
      ->getAdjacents(c)
      ->List.forEach((CoordAndVal((x', y'), cost)) => {
        let v' = node(nodes, x', y')
        //        G.add_edge_e(g, G.E.create(v, cost, v'))

        g->G.addEdge(v, v', ~attr={"weight": cost})
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

    {nodes, g}
  }

  /* * given a nodes in Array2D and graph, find the shortest path from (0,0) to Array2D lengthX/Y */
  let solve = ({nodes, g}) => {
    let dest_x = nodes->Array2D.lengthX - 1
    let dest_y = nodes->Array2D.lengthY - 1
    //    let (_path, w) = Dij.shortest_path(g, node(nodes, 0, 0), node(nodes, dest_x, dest_y))
    let path =
      g->G.ShortestPath.Dijkstra.bidirectional(
        node(nodes, 0, 0),
        node(nodes, dest_x, dest_y),
        ~weight=#Attr("weight"),
      )

    let edges = g->G.ShortestPath.Utils.edgePathFromNodePath(path)

    //    List.forEach(path, e => {
    //      Js.log4("src: ", e->G.E.src->G.V.label, " dst:", e->G.E.dst->G.V.label)
    //    })
    Array.reduce(edges, 0, (acc, e) => {
      acc + g->G.getEdgeAttribute(e, "weight")
    })
  }

  /* * expand Array2D by factor of x_times/ytimes and increase their element values */
  let expand = (map_orig, x_times, y_times) => {
    let sz_x_orig = map_orig->Array2D.lengthX
    let sz_y_orig = map_orig->Array2D.lengthY

    let sz_x_final = sz_x_orig * x_times
    let sz_y_final = sz_y_orig * y_times

    let map = Array2D.make((sz_x_final, sz_y_final), 0)

    for x_ep in 0 to x_times - 1 {
      for y_ep in 0 to y_times - 1 {
        map_orig
        ->Array2D.reduceWithIndex(0, (_a, e, (x', y')) => {
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

  //  let dump_graph = g => {
  //    Js.log("Vertex")
  //    G.iter_vertex(v => {
  //      Js.log2("vertex: ", v)
  //    }, g)
  //    Js.log("Edges")
  //    G.iter_edges_e(e => {
  //      Js.log4("src: ", e->G.E.src, " dst:", e->G.E.dst)
  //    }, g)
  //  }

  //  module Display = {
  //    include G
  //    let vertex_name = (v: V.t) => {
  //      let (x, y) = G.V.label(v)
  //      `"${x->Int.toString},${y->Int.toString}"`
  //    }
  //    let graph_attributes = _ => list{}
  //    let default_vertex_attributes = _ => list{}
  //    let vertex_attributes = _ => list{}
  //    let default_edge_attributes = _ => list{}
  //    let edge_attributes = e => list{#Label(Int.toString(E.label(e)))}
  //    let get_subgraph = _ => None
  //  }

  //  module Gv = Graphviz.Dot(Display)

  @@warning("-3")
  /* * display graph in DOT format */
  //  let output = ({nodes: _, g}) => {
  //    let () = Gv.fprint_graph(Format.str_formatter, g)
  //    Format.flush_str_formatter()
  //  }
}

let parse = data => {
  data
  ->splitNewline
  ->Array.map(l => {
    l->Js.String2.trim->splitChars->Array.map(c => c->intFromStringExn)
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
