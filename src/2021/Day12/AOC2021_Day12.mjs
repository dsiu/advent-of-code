// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Belt_HashMapString from "rescript/lib/es6/belt_HashMapString.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Belt_SortArrayString from "rescript/lib/es6/belt_SortArrayString.js";
import * as Belt_MutableSetString from "rescript/lib/es6/belt_MutableSetString.js";
import * as AdjacencyList$AdventOfCode from "../../AdjacencyList.mjs";

function log(prim) {
  console.log(prim);
}

var ParseError = /* @__PURE__ */Caml_exceptions.create("AOC2021_Day12-AdventOfCode.Maze.ParseError");

function make(edges) {
  var maze = Curry._1(AdjacencyList$AdventOfCode.$$String.make, undefined);
  Belt_Array.forEachU(edges, (function (edge) {
          if (edge.length !== 2) {
            throw {
                  RE_EXN_ID: ParseError,
                  Error: new Error()
                };
          }
          var a = edge[0];
          var b = edge[1];
          AdjacencyList$AdventOfCode.$$String.addEdge(maze, a, b);
          AdjacencyList$AdventOfCode.$$String.addEdge(maze, b, a);
        }));
  return maze;
}

var toString = AdjacencyList$AdventOfCode.$$String.toString;

var Maze = {
  AdjList: undefined,
  ParseError: ParseError,
  make: make,
  toString: toString
};

function is_visited(visited, node) {
  return Belt_Option.mapWithDefaultU(Belt_HashMapString.get(visited, node), false, (function (v) {
                return v > 0;
              }));
}

function inc_visited(visited, node) {
  return Utils$AdventOfCode.hashMapStringUpdate(visited, node, Utils$AdventOfCode.increaseBy1);
}

function get_visited_count(visited, node) {
  return Belt_Option.getWithDefault(Belt_HashMapString.get(visited, node), 0);
}

function is_upper_case(c) {
  return c.toUpperCase() === c;
}

function is_small_case(c) {
  return c.toLowerCase() === c;
}

function has_any_small_cave_been_visited_twice(visited) {
  var smalls = Belt_HashMapString.reduceU(visited, [], (function (acc, key, value) {
          if (key.toLowerCase() === key && value > 1) {
            return Belt_Array.concat(acc, [[
                          key,
                          value
                        ]]);
          } else {
            return acc;
          }
        }));
  return smalls.length !== 0;
}

function get_edges(t, node) {
  return AdjacencyList$AdventOfCode.$$String.neighbors(t, node);
}

function can_visit_part1(visited, node) {
  if (node.toUpperCase() === node) {
    return true;
  } else {
    return get_visited_count(visited, node) <= 0;
  }
}

function can_visit_part2(visited, node) {
  var c = get_visited_count(visited, node);
  if (node.toUpperCase() === node) {
    return true;
  }
  switch (node) {
    case "end" :
    case "start" :
        return c <= 0;
    default:
      if (c <= 0) {
        return true;
      } else {
        return !has_any_small_cave_been_visited_twice(visited);
      }
  }
}

function dfs(visit_func, t, start_node, end_node) {
  var explore = function (node, visited, acc, end_node) {
    var visited$p = inc_visited(visited, node);
    if (node === end_node) {
      return [acc];
    }
    var edges = get_edges(t, node);
    return Belt_MutableSetString.reduceU(edges, [], (function (a, e) {
                  if (Curry._2(visit_func, visited$p, e)) {
                    return Belt_Array.concat(a, explore(e, Belt_HashMapString.copy(visited$p), Belt_Array.concat(acc, [e]), end_node));
                  } else {
                    return a;
                  }
                }));
  };
  var visited = Belt_HashMapString.make(40);
  var acc = [start_node];
  return explore(start_node, visited, acc, end_node);
}

function dfs_part1(param, param$1, param$2) {
  return dfs(can_visit_part1, param, param$1, param$2);
}

function dfs_part2(param, param$1, param$2) {
  return dfs(can_visit_part2, param, param$1, param$2);
}

function array_to_string(xs) {
  return Belt_Array.reduce(xs, "", (function (acc, x) {
                return acc + " " + x;
              }));
}

function sort_result(result) {
  return Belt_SortArrayString.stableSort(Belt_Array.map(result, array_to_string));
}

function parse(data) {
  return Belt_Array.map(Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
                    return prim.trim();
                  })), (function (__x) {
                return __x.split("-");
              }));
}

function solvePart1(data) {
  var maze = make(parse(data));
  return dfs_part1(maze, "start", "end").length;
}

function solvePart2(data) {
  var maze = make(parse(data));
  return dfs_part2(maze, "start", "end").length;
}

var is_big_cave = is_upper_case;

var is_small_cave = is_small_case;

var get_visited_nodes = Belt_HashMapString.keysToArray;

export {
  log ,
  Maze ,
  is_visited ,
  inc_visited ,
  get_visited_count ,
  is_upper_case ,
  is_small_case ,
  is_big_cave ,
  is_small_cave ,
  has_any_small_cave_been_visited_twice ,
  get_edges ,
  can_visit_part1 ,
  can_visit_part2 ,
  get_visited_nodes ,
  dfs ,
  dfs_part1 ,
  dfs_part2 ,
  array_to_string ,
  sort_result ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
