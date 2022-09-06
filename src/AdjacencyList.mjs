// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Id from "rescript/lib/es6/belt_Id.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Belt_HashSet from "rescript/lib/es6/belt_HashSet.js";
import * as Belt_HashMapString from "rescript/lib/es6/belt_HashMapString.js";
import * as Belt_HashSetString from "rescript/lib/es6/belt_HashSetString.js";

function Make(BASE) {
  var containerMake = BASE.containerMake;
  var containerAdd = BASE.containerAdd;
  var containerRemove = BASE.containerRemove;
  var containerHas = BASE.containerHas;
  var containerToArray = BASE.containerToArray;
  var addVertex = function (t, x) {
    var match = Belt_HashMapString.get(t, x);
    if (match !== undefined) {
      return ;
    } else {
      return Belt_HashMapString.set(t, x, Curry._1(containerMake, 40));
    }
  };
  var removeVertex = Belt_HashMapString.remove;
  var getVertex = function (t, x) {
    addVertex(t, x);
    var v = Belt_HashMapString.get(t, x);
    if (v !== undefined) {
      return Caml_option.valFromOption(v);
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
  var addEdge = function (t, x, e) {
    Curry._2(containerAdd, getVertex(t, x), e);
  };
  var removeEdge = function (t, x, y) {
    if (Belt_HashMapString.has(t, x)) {
      return Curry._2(containerRemove, getVertex(t, x), y);
    }
    
  };
  var adjacent = function (t, x, y) {
    var v = Belt_HashMapString.get(t, x);
    if (v !== undefined) {
      return Curry._2(containerHas, Caml_option.valFromOption(v), y);
    } else {
      return false;
    }
  };
  var neighbors = function (t, x) {
    return Belt_Option.getWithDefault(Belt_HashMapString.get(t, x), Curry._1(containerMake, 40));
  };
  var toString = function (t) {
    var str = {
      contents: ""
    };
    Belt_HashMapString.forEachU(t, (function (k, v) {
            str.contents = "" + str.contents + "" + k + ": [ " + Curry._1(containerToArray, v).join(",") + " ]\n";
          }));
    return str.contents;
  };
  return {
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
}

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

function addEdge(t, x, e) {
  Belt_HashSetString.add(getVertex(t, x), e);
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
  Belt_HashMapString.forEachU(t, (function (k, v) {
          str.contents = "" + str.contents + "" + k + ": [ " + Belt_HashSetString.toArray(v).join(",") + " ]\n";
        }));
  return str.contents;
}

var $$String = {
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

var T = {
  eq: eq,
  hash: hash
};

var HashSetTuple = Belt_Id.MakeHashable({
      hash: hash,
      eq: eq
    });

var TupleImpl = {
  T: T,
  HashSetTuple: HashSetTuple
};

function containerMake(hintSize) {
  return Belt_HashSet.make(hintSize, HashSetTuple);
}

function addVertex$1(t, x) {
  var match = Belt_HashMapString.get(t, x);
  if (match !== undefined) {
    return ;
  } else {
    return Belt_HashMapString.set(t, x, containerMake(40));
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

function addEdge$1(t, x, e) {
  Belt_HashSet.add(getVertex$1(t, x), e);
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
  return Belt_Option.getWithDefault(Belt_HashMapString.get(t, x), containerMake(40));
}

function toString$1(t) {
  var str = {
    contents: ""
  };
  Belt_HashMapString.forEachU(t, (function (k, v) {
          str.contents = "" + str.contents + "" + k + ": [ " + Belt_HashSet.toArray(v).join(",") + " ]\n";
        }));
  return str.contents;
}

var Tuple = {
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

export {
  Make ,
  $$String ,
  TupleImpl ,
  Tuple ,
}
/* HashSetTuple Not a pure module */
