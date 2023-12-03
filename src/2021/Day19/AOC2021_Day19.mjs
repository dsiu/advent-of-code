// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Bag$Bag from "rescript-bag/src/bag.mjs";
import * as Belt_Id from "rescript/lib/es6/belt_Id.js";
import * as Belt_Set from "rescript/lib/es6/belt_Set.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Stdlib_List from "@dsiu/rescript-stdlib-fp/src/Stdlib_List.mjs";
import * as Stdlib_Array from "@dsiu/rescript-stdlib-fp/src/Stdlib_Array.mjs";
import * as Stdlib_Function from "@dsiu/rescript-stdlib-fp/src/Stdlib_Function.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Linear$AdventOfCode from "../../Linear.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function coordToString(param) {
  var match = param._0;
  return "(" + String(match[0]) + ", " + String(match[1]) + ", " + String(match[2]) + ")";
}

function transformToString(trans) {
  return coordToString(Curry._1(trans, {
                  TAG: "Coord",
                  _0: [
                    0,
                    0,
                    0
                  ]
                }));
}

function rotX(param) {
  var match = param._0;
  return {
          TAG: "Coord",
          _0: [
            match[0],
            - match[2],
            match[1]
          ]
        };
}

function rotY(param) {
  var match = param._0;
  return {
          TAG: "Coord",
          _0: [
            match[2],
            match[1],
            - match[0]
          ]
        };
}

function rotZ(param) {
  var match = param._0;
  return {
          TAG: "Coord",
          _0: [
            - match[1],
            match[0],
            match[2]
          ]
        };
}

function translate(param, param$1) {
  var match = param$1._0;
  var match$1 = param._0;
  return {
          TAG: "Coord",
          _0: [
            match$1[0] + match[0],
            match$1[1] + match[1],
            match$1[2] + match[2]
          ]
        };
}

var ras = [
  Utils$AdventOfCode.identity,
  rotY,
  (function (param) {
      return Stdlib_Function.compose(rotY, rotY, param);
    }),
  Stdlib_Function.composeN([
        rotY,
        rotY,
        rotY
      ]),
  rotZ,
  Stdlib_Function.composeN([
        rotZ,
        rotZ,
        rotZ
      ])
];

var rbs = [
  Utils$AdventOfCode.identity,
  rotX,
  (function (param) {
      return Stdlib_Function.compose(rotX, rotX, param);
    }),
  Stdlib_Function.composeN([
        rotX,
        rotX,
        rotX
      ])
];

var rotations = Stdlib_Array.combination2(ras, rbs, (function (a, b) {
        return function (param) {
          return Stdlib_Function.compose(a, b, param);
        };
      }));

var compare = Caml_obj.compare;

var I = {
  compare: compare
};

function compare$1(a, b) {
  return a - b | 0;
}

var F = {
  compare: compare$1
};

var B = Bag$Bag.Make(F);

function bagFromArray(__x) {
  return Belt_Array.reduce(__x, B.empty, (function (acc, x) {
                return Curry._3(B.add, x, undefined, acc);
              }));
}

function bagToString(b) {
  var str = {
    contents: ""
  };
  Curry._2(B.iter, (function (x, m) {
          str.contents = str.contents + ("@ " + String(x) + ":" + String(m) + ",");
        }), b);
  return "{" + str.contents + "}";
}

function eq(a, b) {
  return a.scannerName === b.scannerName;
}

function toString(t) {
  return Curry._2(Utils$AdventOfCode.Printable.$$Array.toString, t, (function (param) {
                return "scannerName: " + String(param.scannerName) + ", beacons: " + Curry._2(Utils$AdventOfCode.Printable.$$Array.toString, param.beacons, (function (param) {
                              var match = param._0;
                              return "(" + String(match[0]) + ", " + String(match[1]) + ", " + String(match[2]) + ")";
                            })) + ", signature: " + bagToString(param.signature) + "\n";
              }));
}

function minus(a, b) {
  var b$1 = b._0;
  var a$1 = a._0;
  return {
          TAG: "Coord",
          _0: [
            a$1[0] - b$1[0],
            a$1[1] - b$1[1],
            a$1[2] - b$1[2]
          ]
        };
}

function sign(bcns) {
  var pythag = function (param) {
    var match = param._0;
    var z = match[2];
    var y = match[1];
    var x = match[0];
    return x * x + y * y + z * z;
  };
  return bagFromArray(Stdlib_Array.combinationIf2(bcns, bcns, (function (a, b) {
                    var b$1 = b._0;
                    var a$1 = a._0;
                    if (Linear$AdventOfCode.V3.cmp(b$1, a$1) > 0) {
                      return pythag(minus({
                                      TAG: "Coord",
                                      _0: a$1
                                    }, {
                                      TAG: "Coord",
                                      _0: b$1
                                    }));
                    }
                    
                  })));
}

function vagueMatch(scanner1, scanner2) {
  var s1 = scanner1.signature;
  var s2 = scanner2.signature;
  var s = Belt_List.size(Curry._1(B.elements, Curry._2(B.inter, s1, s2)));
  return s >= 66;
}

function cmp(a, b) {
  return Linear$AdventOfCode.V3.cmp(a._0, b._0);
}

var V3Comparator = Belt_Id.MakeComparable({
      cmp: cmp
    });

function v3SetFromArray(a) {
  return Belt_Set.fromArray(a, V3Comparator);
}

var v3SetMake = Belt_Set.make(V3Comparator);

function interact(a, b) {
  var sa = Belt_Set.fromArray(a, V3Comparator);
  var sb = Belt_Set.fromArray(b, V3Comparator);
  return Belt_Set.intersect(sa, sb);
}

function matchingTransformAll(scanner1, scanner2) {
  var beacons1 = scanner1.beacons;
  var beacons2 = scanner2.beacons;
  return Stdlib_Array.combinationIf3(beacons1, beacons2, rotations, (function (b1, b2, rot) {
                var t = minus(b1, Curry._1(rot, b2));
                var translation = function (param) {
                  return translate(t, param);
                };
                var transB2 = Belt_Array.mapU(beacons2, (function (b) {
                        return translate(t, Curry._1(rot, b));
                      }));
                var len = Belt_Set.size(interact(beacons1, transB2));
                if (len >= 12) {
                  return (function (param) {
                            return Stdlib_Function.compose(rot, translation, param);
                          });
                }
                
              }));
}

function matchingTransform(scanner1, scanner2) {
  return Stdlib_Array.arrayToOption(matchingTransformAll(scanner1, scanner2));
}

function mkReconstruction(scanners) {
  if (scanners) {
    return {
            TAG: "Reconstruction",
            found: /* [] */0,
            working: {
              hd: scanners.hd,
              tl: /* [] */0
            },
            waiting: scanners.tl
          };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "empty scanners",
        Error: new Error()
      };
}

function transformScanner(param) {
  var trans = param[1];
  var s = param[0];
  return {
          scannerName: s.scannerName,
          beacons: Belt_Array.mapU(s.beacons, (function (b) {
                  return Curry._1(Belt_Option.getExn(trans), b);
                })),
          transformation: Belt_Option.getExn(trans),
          signature: s.signature
        };
}

function reconstructStep(param) {
  var working = param.working;
  if (working) {
    var current = working.hd;
    var waiting = param.waiting;
    var passMatches = Belt_List.keep(waiting, (function (x) {
            return vagueMatch(current, x);
          }));
    var matches = Belt_List.keep(Belt_List.zip(passMatches, Belt_List.mapU(passMatches, (function (x) {
                    return Stdlib_Array.arrayToOption(matchingTransformAll(current, x));
                  }))), (function (x) {
            return Belt_Option.isSome(x[1]);
          }));
    var waiting$p = Belt_List.keep(waiting, (function (s) {
            if (Belt_List.has(Belt_List.map(matches, (function (prim) {
                          return prim[0];
                        })), s, eq)) {
              return false;
            } else {
              return true;
            }
          }));
    var newWorker = Belt_List.mapU(matches, transformScanner);
    return {
            TAG: "Reconstruction",
            found: {
              hd: current,
              tl: param.found
            },
            working: Belt_List.concat(working.tl, newWorker),
            waiting: waiting$p
          };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "empty working scanner",
        Error: new Error()
      };
}

function reconstruct(_r) {
  while(true) {
    var r = _r;
    if (!r.working) {
      return r;
    }
    _r = reconstructStep(r);
    continue ;
  };
}

function make(param) {
  var beacons = param[1];
  return {
          scannerName: param[0],
          beacons: beacons,
          transformation: Utils$AdventOfCode.identity,
          signature: sign(beacons)
        };
}

var Scanner = {
  coordToString: coordToString,
  transformToString: transformToString,
  nullTrans: Utils$AdventOfCode.identity,
  rotX: rotX,
  rotY: rotY,
  rotZ: rotZ,
  translate: translate,
  rotations: rotations,
  I: I,
  F: F,
  Bag: undefined,
  B: B,
  bagFromArray: bagFromArray,
  bagToString: bagToString,
  eq: eq,
  toString: toString,
  minus: minus,
  sign: sign,
  vagueMatch: vagueMatch,
  V3Comparator: V3Comparator,
  v3SetFromArray: v3SetFromArray,
  v3SetMake: v3SetMake,
  interact: interact,
  matchingTransformAll: matchingTransformAll,
  matchingTransform: matchingTransform,
  mkReconstruction: mkReconstruction,
  transformScanner: transformScanner,
  reconstructStep: reconstructStep,
  reconstruct: reconstruct,
  make: make
};

function parse(data) {
  var floatFromStr = function (prim) {
    return Number(prim);
  };
  var parseOne = function (data) {
    var lines = Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
            return prim.trim();
          }));
    var name = Utils$AdventOfCode.intFromStringExn(Belt_Array.getExn(lines, 0).replace("--- scanner ", "").replace(" ---", ""));
    var coords = Belt_Array.map(Belt_Array.sliceToEnd(lines, 1), (function (line) {
            var c = Belt_Array.map(line.split(","), floatFromStr);
            return {
                    TAG: "Coord",
                    _0: [
                      Belt_Array.getExn(c, 0),
                      Belt_Array.getExn(c, 1),
                      Belt_Array.getExn(c, 2)
                    ]
                  };
          }));
    return [
            name,
            coords
          ];
  };
  return Belt_Array.map(Utils$AdventOfCode.splitDoubleNewline(data), parseOne);
}

function reconstructScanners(scanners) {
  return reconstruct(mkReconstruction(Belt_List.fromArray(scanners))).found;
}

function part1(scanners) {
  var bSets = Belt_List.mapU(scanners, (function (s) {
          return Belt_Set.fromArray(s.beacons, V3Comparator);
        }));
  return Belt_Set.size(Belt_List.reduceU(bSets, v3SetMake, Belt_Set.union));
}

function part2(scanners) {
  var extractOrigin = function (sc) {
    return Curry._1(sc.transformation, {
                TAG: "Coord",
                _0: [
                  0.0,
                  0.0,
                  0.0
                ]
              });
  };
  var origins = Belt_List.mapU(scanners, extractOrigin);
  return Belt_Option.getExn(Stdlib_List.listToOption(Belt_List.sort(Stdlib_List.combination2(origins, origins, (function (a, b) {
                            var a$1 = minus(a, b);
                            var a$2 = a$1._0;
                            return Math.abs(a$2[0]) + Math.abs(a$2[1]) + Math.abs(a$2[2]);
                          })), (function (a, b) {
                        return b - a | 0;
                      }))));
}

function solvePart1(data) {
  var scanners = Belt_Array.map(parse(data), make);
  return part1(reconstructScanners(scanners));
}

function solvePart2(data) {
  var scanners = Belt_Array.map(parse(data), make);
  return part2(reconstructScanners(scanners));
}

var compose = Stdlib_Function.compose;

var composeN = Stdlib_Function.composeN;

export {
  log ,
  log2 ,
  compose ,
  composeN ,
  Scanner ,
  parse ,
  reconstructScanners ,
  part1 ,
  part2 ,
  solvePart1 ,
  solvePart2 ,
}
/* ras Not a pure module */
