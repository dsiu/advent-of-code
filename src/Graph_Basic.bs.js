// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("rescript/lib/js/list.js");
var Curry = require("rescript/lib/js/curry.js");
var Format = require("rescript/lib/js/format.js");
var Hashtbl = require("rescript/lib/js/hashtbl.js");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Path$Graph = require("rescript-ocamlgraph/src/path.bs.js");
var Components$Graph = require("rescript-ocamlgraph/src/components.bs.js");
var Persistent$Graph = require("rescript-ocamlgraph/src/persistent.bs.js");

var compare = Caml_obj.caml_compare;

var equal = Caml_obj.caml_equal;

var Int = {
  compare: compare,
  hash: Hashtbl.hash,
  equal: equal,
  $$default: 0
};

var partial_arg = Persistent$Graph.Digraph.ConcreteLabeled;

var G = partial_arg(Int, {
      compare: compare,
      $$default: 0
    });

var g = Curry._2(G.add_vertex, G.empty, 1);

var g$1 = Curry._2(G.add_edge_e, g, Curry._3(G.E.create, 1, 10, 2));

var g$2 = Curry._2(G.add_edge_e, g$1, Curry._3(G.E.create, 2, 50, 3));

var g$3 = Curry._2(G.add_edge_e, g$2, Curry._3(G.E.create, 1, 30, 4));

var g$4 = Curry._2(G.add_edge_e, g$3, Curry._3(G.E.create, 1, 100, 5));

var g$5 = Curry._2(G.add_edge_e, g$4, Curry._3(G.E.create, 3, 10, 5));

var g$6 = Curry._2(G.add_edge_e, g$5, Curry._3(G.E.create, 4, 20, 3));

var g$7 = Curry._2(G.add_edge_e, g$6, Curry._3(G.E.create, 4, 60, 5));

var g$8 = Curry._2(G.remove_vertex, g$7, 4);

var gc = Curry._2(G.add_edge_e, g$8, Curry._3(G.E.create, 5, 10, 1));

var gc$1 = Curry._2(G.add_vertex, gc, 6);

function weight(e) {
  return Curry._1(G.E.label, e);
}

function add(prim0, prim1) {
  return prim0 + prim1 | 0;
}

function sub(prim0, prim1) {
  return prim0 - prim1 | 0;
}

var compare$1 = Caml_obj.caml_compare;

var W = {
  weight: weight,
  zero: 0,
  add: add,
  sub: sub,
  compare: compare$1
};

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

var Dij = (function (param) {
      return partial_arg$2(partial_arg$1, param);
    })({
      weight: weight,
      compare: compare$1,
      add: add,
      zero: 0
    });

var match = Curry._3(Dij.shortest_path, gc$1, 1, 5);

var p = match[0];

List.iter((function (e) {
        return Curry._2(Format.printf(/* Format */{
                        _0: {
                          TAG: /* Char_literal */12,
                          _0: /* '[' */91,
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: " -> ",
                              _1: {
                                TAG: /* Int */4,
                                _0: /* Int_d */0,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: {
                                  TAG: /* Char_literal */12,
                                  _0: /* ']' */93,
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        },
                        _1: "[%d -> %d]"
                      }), Curry._1(G.E.src, e), Curry._1(G.E.dst, e));
      }), p);

Format.printf(/* Format */{
      _0: {
        TAG: /* Formatting_lit */17,
        _0: /* Flush_newline */4,
        _1: /* End_of_format */0
      },
      _1: "@."
    });

var Comp = Components$Graph.Make({
      V: G.V,
      iter_vertex: G.iter_vertex,
      iter_succ: G.iter_succ
    });

var g$9 = Curry._3(G.add_edge, g$8, 3, 2);

var match$1 = Curry._1(Comp.scc, g$9);

var f = match$1[1];

var n = match$1[0];

Curry._2(G.iter_edges, (function (u, v) {
        return Curry._2(Format.printf(/* Format */{
                        _0: {
                          TAG: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* String_literal */11,
                            _0: " -> ",
                            _1: {
                              TAG: /* Int */4,
                              _0: /* Int_d */0,
                              _1: /* No_padding */0,
                              _2: /* No_precision */0,
                              _3: {
                                TAG: /* Formatting_lit */17,
                                _0: /* Flush_newline */4,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        },
                        _1: "%d -> %d@."
                      }), u, v);
      }), g$9);

Curry._1(Format.printf(/* Format */{
          _0: {
            TAG: /* Int */4,
            _0: /* Int_d */0,
            _1: /* No_padding */0,
            _2: /* No_precision */0,
            _3: {
              TAG: /* String_literal */11,
              _0: " components",
              _1: {
                TAG: /* Formatting_lit */17,
                _0: /* Flush_newline */4,
                _1: /* End_of_format */0
              }
            }
          },
          _1: "%d components@."
        }), n);

Curry._2(G.iter_vertex, (function (v) {
        return Curry._2(Format.printf(/* Format */{
                        _0: {
                          TAG: /* String_literal */11,
                          _0: "  ",
                          _1: {
                            TAG: /* Int */4,
                            _0: /* Int_d */0,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              TAG: /* String_literal */11,
                              _0: " -> ",
                              _1: {
                                TAG: /* Int */4,
                                _0: /* Int_d */0,
                                _1: /* No_padding */0,
                                _2: /* No_precision */0,
                                _3: {
                                  TAG: /* Formatting_lit */17,
                                  _0: /* Flush_newline */4,
                                  _1: /* End_of_format */0
                                }
                              }
                            }
                          }
                        },
                        _1: "  %d -> %d@."
                      }), v, Curry._1(f, v));
      }), g$9);

var w = match[1];

exports.Int = Int;
exports.G = G;
exports.gc = gc$1;
exports.W = W;
exports.Dij = Dij;
exports.p = p;
exports.w = w;
exports.Comp = Comp;
exports.g = g$9;
exports.n = n;
exports.f = f;
/* G Not a pure module */
