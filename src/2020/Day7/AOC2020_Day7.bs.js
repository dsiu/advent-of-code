// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Int = require("rescript/lib/js/belt_Int.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Belt_MapString = require("rescript/lib/js/belt_MapString.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");

function log(prim) {
  console.log(prim);
  
}

function color(t) {
  return t.color;
}

function count(t) {
  return t.count;
}

function make(count, color) {
  return {
          count: count,
          color: color
        };
}

var empty = {
  count: 0,
  color: ""
};

var Bag = {
  color: color,
  count: count,
  make: make,
  empty: empty
};

var numBagRe = /\D*(\d+)\s+([\w\s]+)\s+bag[s]*.*/i;

var justBagRe = /([\w\s]+)\s+bag[s]*.*/i;

function parseBag(s, r, numIndex, bagIndex) {
  if (s.includes("no other bags")) {
    return empty;
  }
  var y = r.exec(s);
  var c = y !== null ? Belt_Array.map(y, (function (z) {
            return Belt_Option.getExn((z == null) ? undefined : Caml_option.some(z));
          })) : [];
  return {
          count: numIndex === 0 ? 0 : Belt_Option.getExn(Belt_Int.fromString(Belt_Option.getExn(Belt_Array.get(c, numIndex)))),
          color: Belt_Option.getExn(Belt_Array.get(c, bagIndex))
        };
}

function parseNumBag(__x) {
  return parseBag(__x, numBagRe, 1, 2);
}

function parseJustBag(__x) {
  return parseBag(__x, justBagRe, 0, 1);
}

var nodeRe = /(.*)\s+bags/i;

function parseNode(s) {
  var x = nodeRe.exec(s);
  if (x !== null) {
    return Belt_Option.getExn(Belt_Array.get(x, 0));
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var leafRe = /(.*)\s+bags/i;

function parseLeaf(s) {
  var x = leafRe.exec(s);
  if (x !== null) {
    return Belt_Option.getExn(Belt_Array.get(x, 0));
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function addNode(t, node, leaf) {
  return Belt_MapString.set(t, node.color, leaf);
}

function addRule(t, l) {
  var node = parseJustBag(Belt_Option.getExn(Belt_Array.get(l, 0)));
  var leaf = Belt_Array.map(Belt_Option.getExn(Belt_Array.get(l, 1)).split(","), parseNumBag);
  return addNode(t, node, leaf);
}

var Rules = {
  numBagRe: numBagRe,
  justBagRe: justBagRe,
  parseBag: parseBag,
  parseNumBag: parseNumBag,
  parseJustBag: parseJustBag,
  nodeRe: nodeRe,
  parseNode: parseNode,
  leafRe: leafRe,
  parseLeaf: parseLeaf,
  addNode: addNode,
  addRule: addRule,
  make: undefined
};

function parseLine(l) {
  return Belt_Array.map(l.trim().split("contain", 2), (function (prim) {
                return prim.trim();
              }));
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), parseLine);
}

function solvePart1(data) {
  var parsed = Belt_Array.map(Utils$AdventOfCode.splitNewline(data), parseLine);
  var newRules = Belt_Array.reduce(parsed, undefined, addRule);
  Belt_MapString.forEach(newRules, (function (k, v) {
          Utils$AdventOfCode.log(k);
          return Utils$AdventOfCode.log(v);
        }));
  return 1;
}

function solvePart2(data) {
  return 2;
}

exports.log = log;
exports.Bag = Bag;
exports.Rules = Rules;
exports.parseLine = parseLine;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
