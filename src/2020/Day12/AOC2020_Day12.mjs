// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

var InvalidStatus = /* @__PURE__ */Caml_exceptions.create("AOC2020_Day12-AdventOfCode.InvalidStatus");

var make = {
  coord: {
    x: 0,
    y: 0
  },
  facing: "E",
  wayPoint: {
    x: 10,
    y: 1
  }
};

function flip180(facing) {
  if (facing === "N") {
    return "S";
  } else if (facing === "S") {
    return "N";
  } else if (facing === "W") {
    return "E";
  } else {
    return "W";
  }
}

function l90(facing) {
  if (facing === "N") {
    return "W";
  } else if (facing === "S") {
    return "E";
  } else if (facing === "W") {
    return "S";
  } else {
    return "N";
  }
}

function r90(facing) {
  if (facing === "N") {
    return "E";
  } else if (facing === "S") {
    return "W";
  } else if (facing === "W") {
    return "N";
  } else {
    return "S";
  }
}

function rotateShipLeft(t, degree) {
  var match = t.facing;
  var exit = 0;
  if (degree >= 90) {
    if (degree >= 270) {
      if (degree === 360) {
        return t;
      }
      if (degree >= 271) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      exit = 3;
    } else if (degree !== 180) {
      if (degree >= 91) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      exit = 1;
    } else {
      exit = 2;
    }
  } else if (degree !== -270) {
    if (degree !== -180) {
      if (degree !== -90) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      exit = 3;
    } else {
      exit = 2;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        return {
                coord: t.coord,
                facing: l90(match),
                wayPoint: t.wayPoint
              };
    case 2 :
        return {
                coord: t.coord,
                facing: flip180(match),
                wayPoint: t.wayPoint
              };
    case 3 :
        return {
                coord: t.coord,
                facing: r90(match),
                wayPoint: t.wayPoint
              };
    
  }
}

function rotateShipRight(t, degree) {
  var match = t.facing;
  var exit = 0;
  if (degree >= 90) {
    if (degree >= 270) {
      if (degree === 360) {
        return t;
      }
      if (degree >= 271) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      exit = 3;
    } else if (degree !== 180) {
      if (degree >= 91) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      exit = 1;
    } else {
      exit = 2;
    }
  } else if (degree !== -270) {
    if (degree !== -180) {
      if (degree !== -90) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      exit = 3;
    } else {
      exit = 2;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        return {
                coord: t.coord,
                facing: r90(match),
                wayPoint: t.wayPoint
              };
    case 2 :
        return {
                coord: t.coord,
                facing: flip180(match),
                wayPoint: t.wayPoint
              };
    case 3 :
        return {
                coord: t.coord,
                facing: l90(match),
                wayPoint: t.wayPoint
              };
    
  }
}

function move(point, facing, n) {
  if (facing === "N") {
    return {
            x: point.x,
            y: point.y + n | 0
          };
  } else if (facing === "S") {
    return {
            x: point.x,
            y: point.y - n | 0
          };
  } else if (facing === "W") {
    return {
            x: point.x - n | 0,
            y: point.y
          };
  } else {
    return {
            x: point.x + n | 0,
            y: point.y
          };
  }
}

function moveShip(t, direction, n) {
  return {
          coord: move(t.coord, direction, n),
          facing: t.facing,
          wayPoint: t.wayPoint
        };
}

function moveWayPoint(t, direction, n) {
  return {
          coord: t.coord,
          facing: t.facing,
          wayPoint: move(t.wayPoint, direction, n)
        };
}

function moveShipTowardWayPoint(t, n) {
  return {
          coord: {
            x: t.coord.x + Math.imul(t.wayPoint.x, n) | 0,
            y: t.coord.y + Math.imul(t.wayPoint.y, n) | 0
          },
          facing: t.facing,
          wayPoint: t.wayPoint
        };
}

function l90WayPoint(t) {
  return {
          coord: t.coord,
          facing: t.facing,
          wayPoint: {
            x: -t.wayPoint.y | 0,
            y: t.wayPoint.x
          }
        };
}

function r90WayPoint(t) {
  return {
          coord: t.coord,
          facing: t.facing,
          wayPoint: {
            x: t.wayPoint.y,
            y: -t.wayPoint.x | 0
          }
        };
}

function flip180WayPoint(t) {
  return {
          coord: t.coord,
          facing: t.facing,
          wayPoint: {
            x: -t.wayPoint.x | 0,
            y: -t.wayPoint.y | 0
          }
        };
}

function rotateWayPointLeft(t, degree) {
  if (degree >= 90) {
    if (degree >= 270) {
      if (degree === 360) {
        return t;
      }
      if (degree >= 271) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      return r90WayPoint(t);
    }
    if (degree === 180) {
      return flip180WayPoint(t);
    }
    if (degree >= 91) {
      throw {
            RE_EXN_ID: InvalidStatus,
            _1: String(degree),
            Error: new Error()
          };
    }
    return l90WayPoint(t);
  }
  if (degree === -270) {
    return l90WayPoint(t);
  }
  if (degree === -180) {
    return flip180WayPoint(t);
  }
  if (degree !== -90) {
    throw {
          RE_EXN_ID: InvalidStatus,
          _1: String(degree),
          Error: new Error()
        };
  }
  return r90WayPoint(t);
}

function rotateWayPointRight(t, degree) {
  if (degree >= 90) {
    if (degree >= 270) {
      if (degree === 360) {
        return t;
      }
      if (degree >= 271) {
        throw {
              RE_EXN_ID: InvalidStatus,
              _1: String(degree),
              Error: new Error()
            };
      }
      return l90WayPoint(t);
    }
    if (degree === 180) {
      return flip180WayPoint(t);
    }
    if (degree >= 91) {
      throw {
            RE_EXN_ID: InvalidStatus,
            _1: String(degree),
            Error: new Error()
          };
    }
    return r90WayPoint(t);
  }
  if (degree === -270) {
    return r90WayPoint(t);
  }
  if (degree === -180) {
    return flip180WayPoint(t);
  }
  if (degree !== -90) {
    throw {
          RE_EXN_ID: InvalidStatus,
          _1: String(degree),
          Error: new Error()
        };
  }
  return l90WayPoint(t);
}

function execute(ship, s) {
  var variant = s.NAME;
  if (variant === "South") {
    return moveShip(ship, "S", s.VAL);
  } else if (variant === "East") {
    return moveShip(ship, "E", s.VAL);
  } else if (variant === "Left") {
    return rotateShipLeft(ship, s.VAL);
  } else if (variant === "North") {
    return moveShip(ship, "N", s.VAL);
  } else if (variant === "Forward") {
    return moveShip(ship, ship.facing, s.VAL);
  } else if (variant === "West") {
    return moveShip(ship, "W", s.VAL);
  } else {
    return rotateShipRight(ship, s.VAL);
  }
}

function executeWithWayPoint(ship, s) {
  var variant = s.NAME;
  if (variant === "South") {
    return moveWayPoint(ship, "S", s.VAL);
  } else if (variant === "East") {
    return moveWayPoint(ship, "E", s.VAL);
  } else if (variant === "Left") {
    return rotateWayPointLeft(ship, s.VAL);
  } else if (variant === "North") {
    return moveWayPoint(ship, "N", s.VAL);
  } else if (variant === "Forward") {
    return moveShipTowardWayPoint(ship, s.VAL);
  } else if (variant === "West") {
    return moveWayPoint(ship, "W", s.VAL);
  } else {
    return rotateWayPointRight(ship, s.VAL);
  }
}

function make$1(s, n) {
  switch (s) {
    case "E" :
        return {
                NAME: "East",
                VAL: n
              };
    case "F" :
        return {
                NAME: "Forward",
                VAL: n
              };
    case "L" :
        return {
                NAME: "Left",
                VAL: n
              };
    case "N" :
        return {
                NAME: "North",
                VAL: n
              };
    case "R" :
        return {
                NAME: "Right",
                VAL: n
              };
    case "S" :
        return {
                NAME: "South",
                VAL: n
              };
    case "W" :
        return {
                NAME: "West",
                VAL: n
              };
    default:
      throw {
            RE_EXN_ID: InvalidStatus,
            _1: s,
            Error: new Error()
          };
  }
}

var Instruction = {
  execute: execute,
  executeWithWayPoint: executeWithWayPoint,
  make: make$1
};

function execute$1(ship, ops, algo) {
  return Belt_Array.reduce(ops, ship, (function (acc, op) {
                return algo(acc, op);
              }));
}

var Ship = {
  make: make,
  flip180: flip180,
  l90: l90,
  r90: r90,
  rotateShipLeft: rotateShipLeft,
  rotateShipRight: rotateShipRight,
  move: move,
  moveShip: moveShip,
  moveWayPoint: moveWayPoint,
  moveShipTowardWayPoint: moveShipTowardWayPoint,
  l90WayPoint: l90WayPoint,
  r90WayPoint: r90WayPoint,
  flip180WayPoint: flip180WayPoint,
  rotateWayPointLeft: rotateWayPointLeft,
  rotateWayPointRight: rotateWayPointRight,
  Instruction: Instruction,
  execute: execute$1,
  part1Algo: execute,
  part2Algo: executeWithWayPoint
};

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                var s = x.trim();
                var code = s.charAt(0);
                var n = Utils$AdventOfCode.intFromStringExn(s.substring(1));
                return [
                        code,
                        n
                      ];
              }));
}

function solve(data, algo) {
  var ops = Belt_Array.map(parse(data), (function (param) {
          return make$1(param[0], param[1]);
        }));
  var done = execute$1(make, ops, algo);
  return Belt_Array.reduce(Belt_Array.map([
                  done.coord.x,
                  done.coord.y
                ], (function (prim) {
                    return Math.abs(prim);
                  })), 0, Utils$AdventOfCode.add);
}

function solvePart1(__x) {
  return solve(__x, execute);
}

function solvePart2(__x) {
  return solve(__x, executeWithWayPoint);
}

export {
  log ,
  InvalidStatus ,
  Ship ,
  parse ,
  solve ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
