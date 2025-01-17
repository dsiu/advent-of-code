// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Nodefs from "node:fs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.res.mjs";
import * as Belt_MutableQueue from "rescript/lib/es6/Belt_MutableQueue.js";
import * as Belt_MutableStack from "rescript/lib/es6/Belt_MutableStack.js";
import * as Graphology__Graph from "@dsiu/rescript-graphology/src/Graphology__Graph.res.mjs";
import * as Belt_MutableSetString from "rescript/lib/es6/Belt_MutableSetString.js";

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
  let gexfStrWithOptions = G.GEXF.write(g, {
    formatNode: (key, _attributes) => {
      let attr$p = {};
      let attr$p$1 = Object.assign(attr$p, _attributes);
      attr$p$1["name"] = key;
      return {
        label: key,
        attributes: attr$p$1
      };
    },
    formatEdge: (key, _attributes) => {
      let attr$p = {};
      let attr$p$1 = Object.assign(attr$p, _attributes);
      attr$p$1["name"] = key;
      return {
        label: key,
        attributes: {
          name: key
        }
      };
    },
    version: "1.3"
  });
  stringToFile(gexfStrWithOptions, filename);
}

let GEXF = {
  writeToFile: writeToFile
};

let G = Graphology__Graph.MakeGraph({});

function bfs(graph, rootNode, cb) {
  let queue = Belt_MutableQueue.make();
  Belt_MutableQueue.add(queue, {
    TAG: "TraversalRecord",
    node: rootNode,
    depth: 0
  });
  let acc = [];
  let visited = Belt_MutableSetString.make();
  while (true) {
    if (Belt_MutableQueue.isEmpty(queue)) {
      return acc;
    }
    let match = Belt_MutableQueue.popExn(queue);
    let depth = match.depth;
    let node = match.node;
    if (Belt_MutableSetString.has(visited, node)) {
      continue;
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
        _1: (neighbor, _neighborAttr) => Belt_MutableQueue.add(queue, {
          TAG: "TraversalRecord",
          node: neighbor,
          depth: depth + 1 | 0
        })
      });
    }
    continue;
  };
}

function dfs(graph, rootNode, cb) {
  let stack = Belt_MutableStack.make();
  Belt_MutableStack.push(stack, {
    TAG: "TraversalRecord",
    node: rootNode,
    depth: 0
  });
  let acc = [];
  let visited = Belt_MutableSetString.make();
  while (true) {
    if (Belt_MutableStack.isEmpty(stack)) {
      return acc;
    }
    let match = Stdlib__Option.getExn(Belt_MutableStack.pop(stack), undefined);
    let depth = match.depth;
    let node = match.node;
    if (Belt_MutableSetString.has(visited, node)) {
      continue;
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
        _1: (neighbor, _neighborAttr) => Belt_MutableStack.push(stack, {
          TAG: "TraversalRecord",
          node: neighbor,
          depth: depth + 1 | 0
        })
      });
    }
    continue;
  };
}

let Traversal = {
  bfs: bfs,
  dfs: dfs
};

let g = G.makeGraph(undefined);

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

console.log("BFS");

let bfsRes = bfs(g, "1", (node, depth) => {
  G.setNodeAttribute(g, node, "depth", depth);
  return false;
});

console.log("bfsRes", bfsRes);

writeToFile(g, "graph-bfs.gexf", G);

let g$1 = G.makeGraph(undefined);

G.mergeEdge(g$1, "1", "2", undefined);

G.mergeEdge(g$1, "1", "3", undefined);

G.mergeEdge(g$1, "1", "4", undefined);

G.mergeEdge(g$1, "2", "5", undefined);

G.mergeEdge(g$1, "2", "6", undefined);

G.mergeEdge(g$1, "4", "7", undefined);

G.mergeEdge(g$1, "4", "8", undefined);

G.mergeEdge(g$1, "5", "9", undefined);

G.mergeEdge(g$1, "5", "10", undefined);

G.mergeEdge(g$1, "7", "11", undefined);

G.mergeEdge(g$1, "7", "12", undefined);

console.log("DFS");

let dfsRes = dfs(g$1, "1", (node, depth) => {
  G.setNodeAttribute(g$1, node, "depth", depth);
  return false;
});

console.log("dfsRes", dfsRes);

writeToFile(g$1, "graph-dfs.gexf", G);

let Queue;

let Stack;

let $$Set;

export {
  log,
  log2,
  Queue,
  Stack,
  $$Set,
  stringToFile,
  GEXF,
  G,
  Traversal,
}
/* G Not a pure module */