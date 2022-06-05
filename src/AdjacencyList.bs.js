// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Id = require("rescript/lib/js/belt_Id.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Belt_HashSet = require("rescript/lib/js/belt_HashSet.js");
var Belt_HashMapString = require("rescript/lib/js/belt_HashMapString.js");
var Belt_HashSetString = require("rescript/lib/js/belt_HashSetString.js");

function addVertex(t, x) {
  var match = Belt_HashMapString.get(t, x);
  if (match !== undefined) {
    return ;
  } else {
    return Belt_HashMapString.set(t, x, Belt_HashSetString.make(40));
  }
}

var removeVertex = Belt_HashMapString.remove;

function getVertex(t, x) {
  addVertex(t, x);
  var v = Belt_HashMapString.get(t, x);
  if (v !== undefined) {
    return Caml_option.valFromOption(v);
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function addEdge(t, x, y) {
  return Belt_HashSetString.add(getVertex(t, x), y);
}

function removeEdge(t, x, y) {
  if (Belt_HashMapString.has(t, x)) {
    return Belt_HashSetString.remove(getVertex(t, x), y);
  }
  
}

function adjacent(t, x, y) {
  var v = Belt_HashMapString.get(t, x);
  if (v !== undefined) {
    return Belt_HashSetString.has(Caml_option.valFromOption(v), y);
  } else {
    return false;
  }
}

function neighbors(t, x) {
  return Belt_Option.getWithDefault(Belt_HashMapString.get(t, x), Belt_HashSetString.make(40));
}

function toString(t) {
  var str = {
    contents: ""
  };
  Belt_HashMapString.forEach(t, (function (k, v) {
          str.contents = str.contents + k + ": [ " + Belt_HashSetString.toArray(v).join(",") + " ]\n";
          
        }));
  return str.contents;
}

var AdjacencyList_String = {
  make: Belt_HashMapString.make,
  addVertex: addVertex,
  removeVertex: removeVertex,
  getVertex: getVertex,
  addEdge: addEdge,
  removeEdge: removeEdge,
  adjacent: adjacent,
  neighbors: neighbors,
  toString: toString
};

function eq(param, param$1) {
  return param[0] === param$1[0];
}

function hash(param) {
  return caml_hash_final_mix(caml_hash_mix_string(0, param[0]));
}

var HashSetTuple = Belt_Id.MakeHashable({
      hash: hash,
      eq: eq
    });

function addVertex$1(t, x) {
  var match = Belt_HashMapString.get(t, x);
  if (match !== undefined) {
    return ;
  } else {
    return Belt_HashMapString.set(t, x, Belt_HashSet.make(40, HashSetTuple));
  }
}

var removeVertex$1 = Belt_HashMapString.remove;

function getVertex$1(t, x) {
  addVertex$1(t, x);
  var v = Belt_HashMapString.get(t, x);
  if (v !== undefined) {
    return Caml_option.valFromOption(v);
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function addEdge$1(t, x, y) {
  return Belt_HashSet.add(getVertex$1(t, x), y);
}

function removeEdge$1(t, x, y) {
  if (Belt_HashMapString.has(t, x)) {
    return Belt_HashSet.remove(getVertex$1(t, x), y);
  }
  
}

function adjacent$1(t, x, y) {
  var v = Belt_HashMapString.get(t, x);
  if (v !== undefined) {
    return Belt_HashSet.has(Caml_option.valFromOption(v), y);
  } else {
    return false;
  }
}

function neighbors$1(t, x) {
  return Belt_Option.getWithDefault(Belt_HashMapString.get(t, x), Belt_HashSet.make(40, HashSetTuple));
}

function toString$1(t) {
  var str = {
    contents: ""
  };
  Belt_HashMapString.forEach(t, (function (k, v) {
          str.contents = str.contents + k + ": [ " + Belt_HashSet.toArray(v).join(",") + " ]\n";
          
        }));
  return str.contents;
}

var AdjacencyList_Tuple = {
  make: Belt_HashMapString.make,
  addVertex: addVertex$1,
  removeVertex: removeVertex$1,
  getVertex: getVertex$1,
  addEdge: addEdge$1,
  removeEdge: removeEdge$1,
  adjacent: adjacent$1,
  neighbors: neighbors$1,
  toString: toString$1
};

exports.AdjacencyList_String = AdjacencyList_String;
exports.AdjacencyList_Tuple = AdjacencyList_Tuple;
/* HashSetTuple Not a pure module */
