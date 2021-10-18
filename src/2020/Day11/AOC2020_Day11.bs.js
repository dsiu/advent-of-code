// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");
var Array2D$AdventOfCode = require("../../Array2D.bs.js");

function log(prim) {
  console.log(prim);
  
}

var InvalidStatus = /* @__PURE__ */Caml_exceptions.create("AOC2020_Day11-AdventOfCode.SeatMap.InvalidStatus");

function make(c) {
  switch (c) {
    case "#" :
        return "#";
    case "." :
        return ".";
    case "L" :
        return "L";
    default:
      throw {
            RE_EXN_ID: InvalidStatus,
            _1: c,
            Error: new Error()
          };
  }
}

var SeatStatus = {
  make: make
};

function isValidCoord(param, len_x, len_y) {
  var y = param[1];
  var x = param[0];
  if (x >= 0 && x <= (len_x - 1 | 0) && y >= 0) {
    return y <= (len_y - 1 | 0);
  } else {
    return false;
  }
}

function north(param) {
  return [
          param[0],
          param[1] - 1 | 0
        ];
}

function east(param) {
  return [
          param[0] + 1 | 0,
          param[1]
        ];
}

function south(param) {
  return [
          param[0],
          param[1] + 1 | 0
        ];
}

function west(param) {
  return [
          param[0] - 1 | 0,
          param[1]
        ];
}

function northEast(c) {
  return east(north(c));
}

function northWest(c) {
  return west(north(c));
}

function southEast(c) {
  return east(south(c));
}

function southWest(c) {
  return west(south(c));
}

function stepFunc(param, f) {
  return Curry._1(f, [
              param[0],
              param[1]
            ]);
}

function stepN(__x) {
  return stepFunc(__x, north);
}

function stepE(__x) {
  return stepFunc(__x, east);
}

function stepS(__x) {
  return stepFunc(__x, south);
}

function stepW(__x) {
  return stepFunc(__x, west);
}

function stepNE(__x) {
  return stepFunc(__x, northEast);
}

function stepNW(__x) {
  return stepFunc(__x, northWest);
}

function stepSE(__x) {
  return stepFunc(__x, southEast);
}

function stepSW(__x) {
  return stepFunc(__x, southWest);
}

function adjCoords(c) {
  return Belt_Array.map([
              stepNW,
              stepN,
              stepNE,
              stepW,
              stepE,
              stepSW,
              stepS,
              stepSE
            ], (function (f) {
                return Curry._1(f, c);
              }));
}

function getAdjacents(t, param) {
  return Belt_Array.keepMap(adjCoords([
                  param[0],
                  param[1]
                ]), (function (c) {
                if (Array2D$AdventOfCode.isValidXY(t, c)) {
                  return Caml_option.some(Array2D$AdventOfCode.getExn(t, c));
                }
                
              }));
}

function isSeatEq(s, to_be) {
  return s === to_be;
}

function countSeat(xs, seatStatus) {
  return Belt_Array.keep(xs, (function (__x) {
                return __x === seatStatus;
              })).length;
}

function countEmptySeat(__x) {
  return countSeat(__x, "L");
}

function countFloor(__x) {
  return countSeat(__x, ".");
}

function countOccupiedSeat(__x) {
  return countSeat(__x, "#");
}

function transform(s, adjacents) {
  var occupied_seats = countSeat(adjacents, "#");
  if (s === ".") {
    return ".";
  } else if (s === "L") {
    if (occupied_seats === 0) {
      return "#";
    } else {
      return "L";
    }
  } else if (occupied_seats >= 4) {
    return "L";
  } else {
    return "#";
  }
}

function nextSeatIn(t, _param, step) {
  while(true) {
    var param = _param;
    var c = Curry._1(step, [
          param[0],
          param[1]
        ]);
    if (!Array2D$AdventOfCode.isValidXY(t, c)) {
      return ;
    }
    var seat = Belt_Option.getExn(Array2D$AdventOfCode.get(t, c));
    if (seat !== ".") {
      return seat;
    }
    _param = c;
    continue ;
  };
}

function iteratePart1(t) {
  return Array2D$AdventOfCode.mapWithIndex(t, (function (param, s) {
                return transform(s, getAdjacents(t, [
                                param[0],
                                param[1]
                              ]));
              }));
}

function stabilize(_t) {
  while(true) {
    var t = _t;
    var t_next = iteratePart1(t);
    if (Array2D$AdventOfCode.eq(t, t_next)) {
      return t;
    }
    _t = t_next;
    continue ;
  };
}

function make$1(xs) {
  var x = Belt_Array.getExn(xs, 0).length;
  var y = xs.length;
  var ret = Array2D$AdventOfCode.make([
        x,
        y
      ], ".");
  Belt_Array.forEachWithIndex(xs, (function (y, ys) {
          return Belt_Array.forEachWithIndex(Utils$AdventOfCode.splitChars(ys), (function (x, c) {
                        Array2D$AdventOfCode.set(ret, [
                              x,
                              y
                            ], make(c));
                        
                      }));
        }));
  return ret;
}

function dump(t) {
  for(var y = 0 ,y_finish = Array2D$AdventOfCode.lengthY(t); y < y_finish; ++y){
    var prim = Utils$AdventOfCode.join(Belt_Option.getExn(Array2D$AdventOfCode.getYEquals(t, y)));
    console.log(prim);
  }
  
}

var SeatMap = {
  InvalidStatus: InvalidStatus,
  SeatStatus: SeatStatus,
  isValidCoord: isValidCoord,
  north: north,
  east: east,
  south: south,
  west: west,
  northEast: northEast,
  northWest: northWest,
  southEast: southEast,
  southWest: southWest,
  stepFunc: stepFunc,
  stepN: stepN,
  stepE: stepE,
  stepS: stepS,
  stepW: stepW,
  stepNE: stepNE,
  stepNW: stepNW,
  stepSE: stepSE,
  stepSW: stepSW,
  adjCoords: adjCoords,
  getAdjacents: getAdjacents,
  isSeatEq: isSeatEq,
  countSeat: countSeat,
  countEmptySeat: countEmptySeat,
  countFloor: countFloor,
  countOccupiedSeat: countOccupiedSeat,
  transform: transform,
  nextSeatIn: nextSeatIn,
  iteratePart1: iteratePart1,
  isStabilized: Array2D$AdventOfCode.eq,
  stabilize: stabilize,
  make: make$1,
  dump: dump
};

function parse(data) {
  return make$1(Belt_Array.map(data.split("\n"), (function (prim) {
                    return prim.trim();
                  })));
}

function solvePart1(data) {
  var seats = parse(data);
  var result = stabilize(seats);
  return countSeat(Array2D$AdventOfCode.flatten(result), "#");
}

function solvePart2(data) {
  return 2;
}

exports.log = log;
exports.SeatMap = SeatMap;
exports.parse = parse;
exports.solvePart1 = solvePart1;
exports.solvePart2 = solvePart2;
/* No side effect */
