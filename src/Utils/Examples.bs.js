// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Caml_string = require("bs-platform/lib/js/caml_string.js");
var Transducer$AdventOfCode = require("./Transducer.bs.js");

console.log("======================================");

console.log("First example, hard-coded using lists:");

function countAdultsWithInitial(initial, people) {
  return List.length(List.filter(function (person) {
                    return person.age >= 18;
                  })(List.filter(function (person) {
                        return Caml_string.get(person.name, 0) === initial;
                      })(people)));
}

var people = {
  hd: {
    age: 16,
    name: "Alice"
  },
  tl: {
    hd: {
      age: 25,
      name: "Andrew"
    },
    tl: {
      hd: {
        age: 34,
        name: "Ann"
      },
      tl: {
        hd: {
          age: 22,
          name: "Bob"
        },
        tl: /* [] */0
      }
    }
  }
};

console.log(countAdultsWithInitial(/* "A" */65, people));

console.log("==============================================");

console.log("Second example, refactoring into a transducer:");

function $less$less(f, g, x) {
  return Curry._1(f, Curry._1(g, x));
}

function countAdultsWithInitial$1(initial) {
  var g = function (param, param$1, param$2) {
    return Transducer$AdventOfCode.map((function (param) {
                  return 1;
                }), param, param$1, param$2);
  };
  var param = function (param$1, param$2) {
    return g((function (prim, prim$1) {
                  return prim + prim$1 | 0;
                }), param$1, param$2);
  };
  return function (param$1, param$2) {
    var param$3 = function (param$4, param$5) {
      return Transducer$AdventOfCode.filter((function (person) {
                    return person.age >= 18;
                  }), param, param$4, param$5);
    };
    return Transducer$AdventOfCode.filter((function (person) {
                  return Caml_string.get(person.name, 0) === initial;
                }), param$3, param$1, param$2);
  };
}

console.log(List.fold_left(countAdultsWithInitial$1(/* "A" */65), 0, people));

console.log("=======================================================");

console.log("Third example, reusing the pipeline for different ends:");

function adultsWithInitial(initial, combine) {
  var g = function (param, param$1, param$2) {
    return Transducer$AdventOfCode.filter((function (person) {
                  return person.age >= 18;
                }), param, param$1, param$2);
  };
  return function (param, param$1) {
    var param$2 = function (param$3, param$4) {
      return g(combine, param$3, param$4);
    };
    return Transducer$AdventOfCode.filter((function (person) {
                  return Caml_string.get(person.name, 0) === initial;
                }), param$2, param, param$1);
  };
}

console.log("Counting the selected records:");

function countAdultsWithInitial$2(initial) {
  return adultsWithInitial(initial, (function (param, param$1) {
                var param$2 = function (prim, prim$1) {
                  return prim + prim$1 | 0;
                };
                return Transducer$AdventOfCode.map((function (param) {
                              return 1;
                            }), param$2, param, param$1);
              }));
}

console.log(List.fold_left(countAdultsWithInitial$2(/* "A" */65), 0, people));

console.log("Collecting the names of the selected records into a string:");

function join(separator, result, element) {
  return result + (element + separator);
}

function enumerateAdultsWithInitial(initial) {
  var x = function (param, param$1) {
    return join(", ", param, param$1);
  };
  return adultsWithInitial(initial, (function (param, param$1) {
                return Transducer$AdventOfCode.map((function (person) {
                              return person.name;
                            }), x, param, param$1);
              }));
}

console.log(List.fold_left(enumerateAdultsWithInitial(/* "A" */65), "", people));

console.log("Collecting the selected records into a list:");

function append(list, element) {
  return Pervasives.$at(list, {
              hd: element,
              tl: /* [] */0
            });
}

console.log(List.fold_left(adultsWithInitial(/* "A" */65, append), /* [] */0, people));

console.log("===========================================================================");

console.log("Fourth and final example, using the transducer with another data structure:");

function reduce(reducer, _result, _tree) {
  while(true) {
    var tree = _tree;
    var result = _result;
    if (!tree) {
      return result;
    }
    var resultSelf = Curry._2(reducer, result, tree._0);
    var resultLeft = reduce(reducer, resultSelf, tree._1);
    _tree = tree._2;
    _result = resultLeft;
    continue ;
  };
}

var Tree = {
  reduce: reduce
};

var people$1 = /* Node */{
  _0: {
    age: 34,
    name: "Ann"
  },
  _1: /* Node */{
    _0: {
      age: 25,
      name: "Andrew"
    },
    _1: /* Node */{
      _0: {
        age: 16,
        name: "Alice"
      },
      _1: /* Empty */0,
      _2: /* Empty */0
    },
    _2: /* Empty */0
  },
  _2: /* Node */{
    _0: {
      age: 22,
      name: "Bob"
    },
    _1: /* Empty */0,
    _2: /* Empty */0
  }
};

console.log("Counting the selected records:");

console.log(reduce(countAdultsWithInitial$2(/* "A" */65), 0, people$1));

console.log("Collecting the names of the selected records into a string:");

console.log(reduce(enumerateAdultsWithInitial(/* "A" */65), "", people$1));

console.log("Collecting the selected records into a list:");

console.log(reduce(adultsWithInitial(/* "A" */65, append), /* [] */0, people$1));

var T;

exports.T = T;
exports.$less$less = $less$less;
exports.adultsWithInitial = adultsWithInitial;
exports.countAdultsWithInitial = countAdultsWithInitial$2;
exports.join = join;
exports.enumerateAdultsWithInitial = enumerateAdultsWithInitial;
exports.append = append;
exports.Tree = Tree;
exports.people = people$1;
/*  Not a pure module */
