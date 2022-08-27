// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Bag$Bag from "rescript-bag/src/bag.mjs";
import * as Belt_Int from "rescript/lib/es6/belt_Int.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as FP_Utils$AdventOfCode from "../../FP_Utils.mjs";

function log(prim) {
  console.log(prim);
}

function coordToString(param) {
  var match = param._0;
  return "(" + match[0] + ", " + match[1] + ", " + match[2] + ")";
}

function transformToString(trans) {
  return coordToString(Curry._1(trans, /* Coord */{
                  _0: [
                    1,
                    2,
                    3
                  ]
                }));
}

function rotX(param) {
  var match = param._0;
  return /* Coord */{
          _0: [
            match[0],
            -match[2] | 0,
            match[1]
          ]
        };
}

function rotY(param) {
  var match = param._0;
  return /* Coord */{
          _0: [
            match[2],
            match[1],
            -match[0] | 0
          ]
        };
}

function rotZ(param) {
  var match = param._0;
  return /* Coord */{
          _0: [
            -match[1] | 0,
            match[0],
            match[2]
          ]
        };
}

function translate(param, param$1) {
  var match = param$1._0;
  var match$1 = param._0;
  return /* Coord */{
          _0: [
            match$1[0] + match[0] | 0,
            match$1[1] + match[1] | 0,
            match$1[2] + match[2] | 0
          ]
        };
}

var ras = [
  FP_Utils$AdventOfCode.identity,
  rotY,
  (function (param) {
      return FP_Utils$AdventOfCode.compose(rotY, rotY, param);
    }),
  FP_Utils$AdventOfCode.composeN([
        rotY,
        rotY,
        rotY
      ]),
  rotZ,
  FP_Utils$AdventOfCode.composeN([
        rotZ,
        rotZ,
        rotZ
      ])
];

var rbs = [
  FP_Utils$AdventOfCode.identity,
  rotX,
  (function (param) {
      return FP_Utils$AdventOfCode.compose(rotX, rotX, param);
    }),
  FP_Utils$AdventOfCode.composeN([
        rotX,
        rotX,
        rotX
      ])
];

var rotations = FP_Utils$AdventOfCode.combinationArray2(ras, rbs, (function (a, b) {
        return function (param) {
          return FP_Utils$AdventOfCode.compose(a, b, param);
        };
      }));

console.log(rotations.length);

var prim = Curry._2(Utils$AdventOfCode.Printable.$$Array.toString, rotations, transformToString);

console.log(prim);

var compare = Caml_obj.compare;

var I = {
  compare: compare
};

var B = Bag$Bag.Make(I);

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
          str.contents = str.contents + ("@ " + x + ":" + m + ",");
        }), b);
  return "{" + str.contents + "}";
}

function toString(t) {
  return Curry._2(Utils$AdventOfCode.Printable.$$Array.toString, t, (function (param) {
                return "scannerName: " + param.scannerName + ", beacons: " + Curry._2(Utils$AdventOfCode.Printable.$$Array.toString, param.beacons, (function (param) {
                              var match = param._0;
                              return "(" + match[0] + ", " + match[1] + ", " + match[2] + ")";
                            })) + "\n" + ("signature: " + bagToString(param.signature) + "") + "\n";
              }));
}

function sign(bcns) {
  var pythag = function (param) {
    var match = param._0;
    var z = match[2];
    var y = match[1];
    var x = match[0];
    return (Math.imul(x, x) + Math.imul(y, y) | 0) + Math.imul(z, z) | 0;
  };
  return bagFromArray(FP_Utils$AdventOfCode.combinationIfArray2(bcns, bcns, (function (a, b) {
                    if (Caml_obj.lessthan(a, b)) {
                      return pythag(a) - pythag(b) | 0;
                    }
                    
                  })));
}

function make(param) {
  var beacons = param[1];
  return {
          scannerName: param[0],
          beacons: beacons,
          transformation: FP_Utils$AdventOfCode.identity,
          signature: sign(beacons)
        };
}

var Scanner = {
  coordToString: coordToString,
  transformToString: transformToString,
  nullTrans: FP_Utils$AdventOfCode.identity,
  rotX: rotX,
  rotY: rotY,
  rotZ: rotZ,
  translate: translate,
  rotations: rotations,
  I: I,
  Bag: undefined,
  B: B,
  bagFromArray: bagFromArray,
  bagToString: bagToString,
  toString: toString,
  sign: sign,
  make: make
};

function parse(data) {
  var parseOne = function (data) {
    var lines = Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
            return prim.trim();
          }));
    var name = Belt_Option.getExn(Belt_Int.fromString(Belt_Array.getExn(lines, 0).replace("--- scanner ", "").replace(" ---", "")));
    var coords = Belt_Array.map(Belt_Array.sliceToEnd(lines, 1), (function (line) {
            var c = Belt_Array.map(line.split(","), Utils$AdventOfCode.intFromStringExn);
            return /* Coord */{
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

function solvePart1(data) {
  var scanners = Belt_Array.map(parse(data), make);
  var prim = toString(scanners);
  console.log(prim);
  return 1;
}

function solvePart2(data) {
  return 2;
}

export {
  log ,
  Scanner ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* ras Not a pure module */
