// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";
import * as Coordinate$AdventOfCode from "../../Coordinate.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function makeElem($$char) {
  var d = Stdlib__Int.fromString(10, $$char);
  if (d !== undefined) {
    return {
            TAG: "Digit",
            _0: d
          };
  } else if ($$char === ".") {
    return "Dot";
  } else {
    return {
            TAG: "Symbol",
            _0: $$char
          };
  }
}

function isDigit(e) {
  if (typeof e !== "object" || e.TAG === "Symbol") {
    return false;
  } else {
    return true;
  }
}

function isSymbol(e) {
  if (typeof e !== "object" || e.TAG !== "Symbol") {
    return false;
  } else {
    return true;
  }
}

function isStar(e) {
  if (typeof e !== "object" || !(e.TAG === "Symbol" && e._0 === "*")) {
    return false;
  } else {
    return true;
  }
}

function getNeighborsIf(c, fn) {
  var directions = [
    Coordinate$AdventOfCode.Direction.north,
    Coordinate$AdventOfCode.Direction.south,
    Coordinate$AdventOfCode.Direction.east,
    Coordinate$AdventOfCode.Direction.west,
    Coordinate$AdventOfCode.Direction.northEast,
    Coordinate$AdventOfCode.Direction.northWest,
    Coordinate$AdventOfCode.Direction.southEast,
    Coordinate$AdventOfCode.Direction.southWest
  ];
  return Stdlib__Array.filterMap(directions, (function (dir) {
                return fn(dir(c));
              }));
}

function isElemDigit(engine, p) {
  return Stdlib__Option.flatMap(Array2D$AdventOfCode.get(engine, p), (function (e) {
                if (isDigit(e)) {
                  return p;
                }
                
              }));
}

function makeEngine(__x) {
  return Array2D$AdventOfCode.map(__x, makeElem);
}

function engineFilter(engine, fn) {
  return Array2D$AdventOfCode.reduceWithIndex(engine, [], (function (acc, e, c) {
                if (fn(e)) {
                  acc.push(c);
                }
                return acc;
              }));
}

function rowsFromRegion(region) {
  return Stdlib__Array.reduce(region.map(function (param) {
                    return param[1];
                  }).toSorted(Stdlib__Int.compare), [], (function (acc, e) {
                var last = acc.at(acc.length - 1 | 0);
                if (last !== undefined && last === e) {
                  return acc;
                } else {
                  acc.push(e);
                  return acc;
                }
              }));
}

function findNumbers(engine) {
  var numberRegions = engineFilter(engine, isDigit).toSorted(Coordinate$AdventOfCode.Compare.yx);
  var rowsWithNumber = rowsFromRegion(numberRegions);
  var groupedByRow = rowsWithNumber.map(function (y) {
        return numberRegions.filter(function (param) {
                    return param[1] === y;
                  });
      });
  return groupedByRow.map(function (__x) {
                  return __x.toSorted(Coordinate$AdventOfCode.Compare.xy);
                }).map(function (l) {
                l.push([
                      0,
                      0
                    ]);
                return Stdlib__Array.reduce(l, [
                              [],
                              []
                            ], (function (param, param$1) {
                                var y = param$1[1];
                                var x = param$1[0];
                                var buf = param[1];
                                var parsed = param[0];
                                var last = buf.at(buf.length - 1 | 0);
                                if (last !== undefined && last[0] !== (x - 1 | 0)) {
                                  parsed.push(buf);
                                  return [
                                          parsed,
                                          [[
                                              x,
                                              y
                                            ]]
                                        ];
                                } else {
                                  buf.push([
                                        x,
                                        y
                                      ]);
                                  return [
                                          parsed,
                                          buf
                                        ];
                                }
                              }))[0];
              }).flat();
}

function findSymbols(__x) {
  return engineFilter(__x, isSymbol);
}

function findStars(__x) {
  return engineFilter(__x, isStar);
}

function touchedDigit(engine, symbols) {
  return symbols.flatMap(function (__x) {
              return getNeighborsIf(__x, (function (__x) {
                            return isElemDigit(engine, __x);
                          }));
            });
}

function isNumberTouched(number, symTouched) {
  return symTouched.some(function (sPos) {
              return number.some(function (nPos) {
                          return Caml_obj.equal(nPos, sPos);
                        });
            });
}

function getNumber(engine, number) {
  return Stdlib__Array.reduce(number.map(function (n) {
                  var match = Array2D$AdventOfCode.get(engine, n);
                  if (match !== undefined && !(typeof match !== "object" || match.TAG === "Symbol")) {
                    return match._0;
                  } else {
                    return PervasivesU.failwith("expected digit");
                  }
                }), 0, (function (acc, d) {
                return Math.imul(acc, 10) + d | 0;
              }));
}

function findNumbersTouched(engine, numbers, symTouched) {
  return Stdlib__Array.filterMap(numbers, (function (num) {
                if (isNumberTouched(num, symTouched)) {
                  return getNumber(engine, num);
                }
                
              }));
}

function part1(engine) {
  var symbols = engineFilter(engine, isSymbol);
  var numbers = findNumbers(engine);
  var symTouched = touchedDigit(engine, symbols);
  var nums = findNumbersTouched(engine, numbers, symTouched);
  return Utils$AdventOfCode.sumIntArray(nums);
}

function part2(engine) {
  var stars = engineFilter(engine, isStar);
  var numbers = findNumbers(engine);
  var starTouched = Stdlib__Array.filterMap(stars, (function (s) {
          var t = touchedDigit(engine, [s]);
          if (t.length >= 2) {
            return t;
          }
          
        }));
  var gearParts = Stdlib__Array.filterMap(starTouched, (function (x) {
          var touchedNums = findNumbersTouched(engine, numbers, x);
          if (touchedNums.length === 2) {
            return Utils$AdventOfCode.mulIntArray(touchedNums);
          }
          
        }));
  return Utils$AdventOfCode.sumIntArray(gearParts);
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(Utils$AdventOfCode.compose((function (prim) {
                    return prim.trim();
                  }), Utils$AdventOfCode.splitChars));
}

function solvePart1(data) {
  var __x = parse(data);
  return part1(Array2D$AdventOfCode.map(__x, makeElem));
}

function solvePart2(data) {
  var __x = parse(data);
  return part2(Array2D$AdventOfCode.map(__x, makeElem));
}

export {
  log ,
  log2 ,
  makeElem ,
  isDigit ,
  isSymbol ,
  isStar ,
  getNeighborsIf ,
  isElemDigit ,
  makeEngine ,
  engineFilter ,
  rowsFromRegion ,
  findNumbers ,
  findSymbols ,
  findStars ,
  touchedDigit ,
  isNumberTouched ,
  getNumber ,
  findNumbersTouched ,
  part1 ,
  part2 ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* Stdlib__Int Not a pure module */
