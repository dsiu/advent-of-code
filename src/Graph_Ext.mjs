// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Nodefs from "node:fs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Belt_MutableQueue from "rescript/lib/es6/belt_MutableQueue.js";
import * as Belt_MutableStack from "rescript/lib/es6/belt_MutableStack.js";
import * as Graphology__Graph from "@dsiu/rescript-graphology/src/Graphology__Graph.mjs";
import * as Belt_MutableSetString from "rescript/lib/es6/belt_MutableSetString.js";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function stringToFile(str, fileName) {
  Nodefs.writeFileSync(fileName, Buffer.from(str));
}

function writeToFile(g, filename, G) {
  var gexfStrWithOptions = G.GEXF.write(g, {
        formatNode: (function (key, _attributes) {
            var attr$p = {};
            var attr$p$1 = Object.assign(attr$p, _attributes);
            attr$p$1["name"] = key;
            return {
                    label: key,
                    attributes: attr$p$1
                  };
          }),
        formatEdge: (function (key, _attributes) {
            var attr$p = {};
            var attr$p$1 = Object.assign(attr$p, _attributes);
            attr$p$1["name"] = key;
            return {
                    label: key,
                    attributes: {
                      name: key
                    }
                  };
          }),
        version: "1.3"
      });
  stringToFile(gexfStrWithOptions, filename);
}

var GEXF = {
  writeToFile: writeToFile
};

var G = Graphology__Graph.MakeGraph({});

function bfs(graph, rootNode, cb) {
  var queue = Belt_MutableQueue.make();
  Belt_MutableQueue.add(queue, {
        TAG: "TraversalRecord",
        node: rootNode,
        depth: 0
      });
  var acc = [];
  var visited = Belt_MutableSetString.make();
  while(true) {
    if (Belt_MutableQueue.isEmpty(queue)) {
      return acc;
    }
    var match = Belt_MutableQueue.popExn(queue);
    var depth = match.depth;
    var node = match.node;
    if (Belt_MutableSetString.has(visited, node)) {
      continue ;
    }
    acc.push({
          TAG: "TraversalRecord",
          node: node,
          depth: depth
        });
    Belt_MutableSetString.add(visited, node);
    if (!cb(node, depth)) {
      G.NeighborsIter.forEachOutboundNeighbor(graph, {
            TAG: "Node",
            _0: node,
            _1: (function(depth){
            return function (neighbor, _neighborAttr) {
              Belt_MutableQueue.add(queue, {
                    TAG: "TraversalRecord",
                    node: neighbor,
                    depth: depth + 1 | 0
                  });
            }
            }(depth))
          });
    }
    continue ;
  };
}

function dfs(graph, rootNode, cb) {
  var stack = Belt_MutableStack.make();
  Belt_MutableStack.push(stack, {
        TAG: "TraversalRecord",
        node: rootNode,
        depth: 0
      });
  var acc = [];
  var visited = Belt_MutableSetString.make();
  while(true) {
    if (Belt_MutableStack.isEmpty(stack)) {
      return acc;
    }
    var match = Stdlib__Option.getExn(Belt_MutableStack.pop(stack));
    var depth = match.depth;
    var node = match.node;
    if (Belt_MutableSetString.has(visited, node)) {
      continue ;
    }
    acc.push({
          TAG: "TraversalRecord",
          node: node,
          depth: depth
        });
    Belt_MutableSetString.add(visited, node);
    if (!cb(node, depth)) {
      G.NeighborsIter.forEachOutboundNeighbor(graph, {
            TAG: "Node",
            _0: node,
            _1: (function(depth){
            return function (neighbor, _neighborAttr) {
              Belt_MutableStack.push(stack, {
                    TAG: "TraversalRecord",
                    node: neighbor,
                    depth: depth + 1 | 0
                  });
            }
            }(depth))
          });
    }
    continue ;
  };
}

var Traversal = {
  bfs: bfs,
  dfs: dfs
};

var g = G.makeGraph(undefined);

G.mergeEdge(g, "1", "2", undefined);

G.mergeEdge(g, "1", "3", undefined);

G.mergeEdge(g, "1", "4", undefined);

G.mergeEdge(g, "2", "5", undefined);

G.mergeEdge(g, "2", "6", undefined);

G.mergeEdge(g, "4", "7", undefined);

G.mergeEdge(g, "4", "8", undefined);

G.mergeEdge(g, "5", "9", undefined);

G.mergeEdge(g, "5", "10", undefined);

G.mergeEdge(g, "7", "11", undefined);

G.mergeEdge(g, "7", "12", undefined);

var prim = G.inspect(g);

console.log(prim);

console.log("BFS");

var bfsRes = bfs(g, "1", (function (node, depth) {
        console.log(node, depth);
        G.setNodeAttribute(g, node, "depth", depth);
        return false;
      }));

console.log(bfsRes, "bfsRes");

console.log("DFS");

var dfsRes = dfs(g, "1", (function (node, depth) {
        console.log(node, depth);
        return false;
      }));

console.log(dfsRes, "dfsRes");

writeToFile(g, "graph.gexf", G);

var Queue;

var Stack;

var $$Set;

export {
  log ,
  log2 ,
  Queue ,
  Stack ,
  $$Set ,
  stringToFile ,
  GEXF ,
  G ,
  Traversal ,
}
/* G Not a pure module */
