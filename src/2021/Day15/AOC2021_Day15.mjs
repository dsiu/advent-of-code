// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Path$Graph from "rescript-ocamlgraph/src/path.mjs";
import * as Imperative$Graph from "rescript-ocamlgraph/src/imperative.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";
import * as Coordinate$AdventOfCode from "../../Coordinate.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var compare = Caml_obj.compare;

var E = {
  compare: compare,
  $$default: 0
};

var V = {};

function W(E) {
  var add = function (prim0, prim1) {
    return prim0 + prim1 | 0;
  };
  var compare = Caml_obj.compare;
  return {
          weight: E.label,
          zero: 0,
          add: add,
          compare: compare
        };
}

var partial_arg = Imperative$Graph.Digraph.AbstractLabeled;

var G = partial_arg(V, E);

var $$let = G.E;

var partial_arg_V = G.V;

var partial_arg_E = {
  label: $$let.label,
  src: $$let.src,
  dst: $$let.dst,
  create: $$let.create
};

var partial_arg_iter_vertex = G.iter_vertex;

var partial_arg_fold_vertex = G.fold_vertex;

var partial_arg_iter_succ = G.iter_succ;

var partial_arg_iter_succ_e = G.iter_succ_e;

var partial_arg_fold_edges_e = G.fold_edges_e;

var partial_arg_nb_vertex = G.nb_vertex;

var partial_arg$1 = {
  V: partial_arg_V,
  E: partial_arg_E,
  iter_vertex: partial_arg_iter_vertex,
  fold_vertex: partial_arg_fold_vertex,
  iter_succ: partial_arg_iter_succ,
  iter_succ_e: partial_arg_iter_succ_e,
  fold_edges_e: partial_arg_fold_edges_e,
  nb_vertex: partial_arg_nb_vertex
};

var partial_arg$2 = Path$Graph.Dijkstra;

function add(prim0, prim1) {
  return prim0 + prim1 | 0;
}

var compare$1 = Caml_obj.compare;

var Dij = (function (param) {
      return partial_arg$2(partial_arg$1, param);
    })({
      weight: G.E.label,
      compare: compare$1,
      add: add,
      zero: 0
    });

function adjCoords(c) {
  return Belt_List.map({
              hd: Coordinate$AdventOfCode.stepN,
              tl: {
                hd: Coordinate$AdventOfCode.stepW,
                tl: {
                  hd: Coordinate$AdventOfCode.stepE,
                  tl: {
                    hd: Coordinate$AdventOfCode.stepS,
                    tl: /* [] */0
                  }
                }
              }
            }, (function (f) {
                return f(c);
              }));
}

function getAdjacents(t, param) {
  return Belt_List.keepMapU(adjCoords([
                  param[0],
                  param[1]
                ]), (function (c) {
                if (Array2D$AdventOfCode.isValidXY(t, c)) {
                  return {
                          TAG: "CoordAndVal",
                          _0: c,
                          _1: Array2D$AdventOfCode.getExn(t, c)
                        };
                }
                
              }));
}

function makeNodes(g, lines) {
  return Array2D$AdventOfCode.mapWithIndexU(lines, (function (c, param) {
                var v = Curry._1(G.V.create, c);
                Curry._2(G.add_vertex, g, v);
                return v;
              }));
}

function node(nodes, x, y) {
  return Array2D$AdventOfCode.getExn(nodes, [
              x,
              y
            ]);
}

function make(lines) {
  var g = Curry._2(G.create, undefined, undefined);
  var nodes = makeNodes(g, lines);
  var g$1 = Array2D$AdventOfCode.reduceWithIndexU(lines, g, (function (g, _e, c) {
          var v = node(nodes, c[0], c[1]);
          Belt_List.forEach(getAdjacents(lines, c), (function (param) {
                  var match = param._0;
                  var v$p = node(nodes, match[0], match[1]);
                  Curry._2(G.add_edge_e, g, Curry._3(G.E.create, v, param._1, v$p));
                }));
          return g;
        }));
  return {
          nodes: nodes,
          g: g$1
        };
}

function solve(param) {
  var nodes = param.nodes;
  var dest_x = Array2D$AdventOfCode.lengthX(nodes) - 1 | 0;
  var dest_y = Array2D$AdventOfCode.lengthY(nodes) - 1 | 0;
  return Curry._3(Dij.shortest_path, param.g, node(nodes, 0, 0), node(nodes, dest_x, dest_y))[1];
}

function expand(map_orig, x_times, y_times) {
  var sz_x_orig = Array2D$AdventOfCode.lengthX(map_orig);
  var sz_y_orig = Array2D$AdventOfCode.lengthY(map_orig);
  var sz_x_final = Math.imul(sz_x_orig, x_times);
  var sz_y_final = Math.imul(sz_y_orig, y_times);
  var map = Array2D$AdventOfCode.make([
        sz_x_final,
        sz_y_final
      ], 0);
  for(var x_ep = 0; x_ep < x_times; ++x_ep){
    (function(x_ep){
    for(var y_ep = 0; y_ep < y_times; ++y_ep){
      Array2D$AdventOfCode.reduceWithIndex(map_orig, 0, (function(y_ep){
          return function (_a, e, param) {
            var x_new = param[0] + Math.imul(x_ep, sz_x_orig) | 0;
            var y_new = param[1] + Math.imul(y_ep, sz_y_orig) | 0;
            var e$p = ((e + x_ep | 0) + y_ep | 0) % 9;
            var e_new = e$p === 0 ? 9 : e$p;
            Array2D$AdventOfCode.set(map, [
                  x_new,
                  y_new
                ], e_new);
            return 0;
          }
          }(y_ep)));
    }
    }(x_ep));
  }
  return map;
}

function dump_graph(g) {
  console.log("Vertex");
  Curry._2(G.iter_vertex, (function (v) {
          console.log("vertex: ", v);
        }), g);
  console.log("Edges");
  Curry._2(G.iter_edges_e, (function (e) {
          console.log("src: ", Curry._1(G.E.src, e), " dst:", Curry._1(G.E.dst, e));
        }), g);
}

function vertex_name(v) {
  var match = Curry._1(G.V.label, v);
  return "\"" + String(match[0]) + "," + String(match[1]) + "\"";
}

function graph_attributes(param) {
  return /* [] */0;
}

function default_vertex_attributes(param) {
  return /* [] */0;
}

function vertex_attributes(param) {
  return /* [] */0;
}

function default_edge_attributes(param) {
  return /* [] */0;
}

function edge_attributes(e) {
  return {
          hd: {
            NAME: "Label",
            VAL: String(Curry._1(G.E.label, e))
          },
          tl: /* [] */0
        };
}

function get_subgraph(param) {
  
}

var Display_V = G.V;

var Display_E = G.E;

var Display_is_directed = G.is_directed;

var Display_is_empty = G.is_empty;

var Display_nb_vertex = G.nb_vertex;

var Display_nb_edges = G.nb_edges;

var Display_out_degree = G.out_degree;

var Display_in_degree = G.in_degree;

var Display_mem_vertex = G.mem_vertex;

var Display_mem_edge = G.mem_edge;

var Display_mem_edge_e = G.mem_edge_e;

var Display_find_edge = G.find_edge;

var Display_find_all_edges = G.find_all_edges;

var Display_succ = G.succ;

var Display_pred = G.pred;

var Display_succ_e = G.succ_e;

var Display_pred_e = G.pred_e;

var Display_iter_vertex = G.iter_vertex;

var Display_fold_vertex = G.fold_vertex;

var Display_iter_edges = G.iter_edges;

var Display_fold_edges = G.fold_edges;

var Display_iter_edges_e = G.iter_edges_e;

var Display_fold_edges_e = G.fold_edges_e;

var Display_map_vertex = G.map_vertex;

var Display_iter_succ = G.iter_succ;

var Display_iter_pred = G.iter_pred;

var Display_fold_succ = G.fold_succ;

var Display_fold_pred = G.fold_pred;

var Display_iter_succ_e = G.iter_succ_e;

var Display_fold_succ_e = G.fold_succ_e;

var Display_iter_pred_e = G.iter_pred_e;

var Display_fold_pred_e = G.fold_pred_e;

var Display_create = G.create;

var Display_clear = G.clear;

var Display_copy = G.copy;

var Display_add_vertex = G.add_vertex;

var Display_remove_vertex = G.remove_vertex;

var Display_add_edge = G.add_edge;

var Display_add_edge_e = G.add_edge_e;

var Display_remove_edge = G.remove_edge;

var Display_remove_edge_e = G.remove_edge_e;

var Display_Mark = G.Mark;

var Display = {
  V: Display_V,
  E: Display_E,
  is_directed: Display_is_directed,
  is_empty: Display_is_empty,
  nb_vertex: Display_nb_vertex,
  nb_edges: Display_nb_edges,
  out_degree: Display_out_degree,
  in_degree: Display_in_degree,
  mem_vertex: Display_mem_vertex,
  mem_edge: Display_mem_edge,
  mem_edge_e: Display_mem_edge_e,
  find_edge: Display_find_edge,
  find_all_edges: Display_find_all_edges,
  succ: Display_succ,
  pred: Display_pred,
  succ_e: Display_succ_e,
  pred_e: Display_pred_e,
  iter_vertex: Display_iter_vertex,
  fold_vertex: Display_fold_vertex,
  iter_edges: Display_iter_edges,
  fold_edges: Display_fold_edges,
  iter_edges_e: Display_iter_edges_e,
  fold_edges_e: Display_fold_edges_e,
  map_vertex: Display_map_vertex,
  iter_succ: Display_iter_succ,
  iter_pred: Display_iter_pred,
  fold_succ: Display_fold_succ,
  fold_pred: Display_fold_pred,
  iter_succ_e: Display_iter_succ_e,
  fold_succ_e: Display_fold_succ_e,
  iter_pred_e: Display_iter_pred_e,
  fold_pred_e: Display_fold_pred_e,
  create: Display_create,
  clear: Display_clear,
  copy: Display_copy,
  add_vertex: Display_add_vertex,
  remove_vertex: Display_remove_vertex,
  add_edge: Display_add_edge,
  add_edge_e: Display_add_edge_e,
  remove_edge: Display_remove_edge,
  remove_edge_e: Display_remove_edge_e,
  Mark: Display_Mark,
  vertex_name: vertex_name,
  graph_attributes: graph_attributes,
  default_vertex_attributes: default_vertex_attributes,
  vertex_attributes: vertex_attributes,
  default_edge_attributes: default_edge_attributes,
  edge_attributes: edge_attributes,
  get_subgraph: get_subgraph
};

var Cave = {
  E: E,
  V: V,
  W: W,
  G: G,
  Dij: Dij,
  adjCoords: adjCoords,
  getAdjacents: getAdjacents,
  makeNodes: makeNodes,
  node: node,
  make: make,
  solve: solve,
  expand: expand,
  dump_graph: dump_graph,
  Display: Display
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (l) {
                return Belt_Array.map(Utils$AdventOfCode.splitChars(l.trim()), Utils$AdventOfCode.intFromStringExn);
              }));
}

function solvePart1(data) {
  return solve(make(parse(data)));
}

function solvePart2(data) {
  var new_cave = expand(parse(data), 5, 5);
  return solve(make(new_cave));
}

export {
  log ,
  log2 ,
  Cave ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* G Not a pure module */
