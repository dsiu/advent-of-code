// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Array2D$AdventOfCode from "../../Array2D.mjs";
import * as Coordinate$AdventOfCode from "../../Coordinate.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function makeElem(char) {
  let d = Stdlib__Int.fromString(char, 10);
  if (d !== undefined) {
    return {
      TAG: "Digit",
      _0: d
    };
  } else if (char === ".") {
    return "Dot";
  } else {
    return {
      TAG: "Symbol",
      _0: char
    };
  }
}

function isDigit(e) {
  if (typeof e !== "object") {
    return false;
  } else {
    return e.TAG !== "Symbol";
  }
}

function isSymbol(e) {
  if (typeof e !== "object") {
    return false;
  } else {
    return e.TAG === "Symbol";
  }
}

function isStar(e) {
  if (typeof e !== "object" || e.TAG !== "Symbol") {
    return false;
  } else {
    return e._0 === "*";
  }
}

function getNeighborsIf(c, fn) {
  let directions = [
    Coordinate$AdventOfCode.Direction.north,
    Coordinate$AdventOfCode.Direction.south,
    Coordinate$AdventOfCode.Direction.east,
    Coordinate$AdventOfCode.Direction.west,
    Coordinate$AdventOfCode.Direction.northEast,
    Coordinate$AdventOfCode.Direction.northWest,
    Coordinate$AdventOfCode.Direction.southEast,
    Coordinate$AdventOfCode.Direction.southWest
  ];
  return Stdlib__Array.filterMap(directions, dir => fn(dir(c)));
}

function isElemDigit(engine, p) {
  return Stdlib__Option.flatMap(Array2D$AdventOfCode.get(engine, p), e => {
    if (isDigit(e)) {
      return p;
    }
    
  });
}

function makeEngine(__x) {
  return Array2D$AdventOfCode.map(__x, makeElem);
}

function engineFilter(engine, fn) {
  return Array2D$AdventOfCode.reduceWithIndex(engine, [], (acc, e, c) => {
    if (fn(e)) {
      acc.push(c);
    }
    return acc;
  });
}

function rowsFromRegion(region) {
  return Stdlib__Array.reduce(region.map(param => param[1]).toSorted(Primitive_int.compare), [], (acc, e) => {
    let last = acc.at(acc.length - 1 | 0);
    if (last !== undefined && last === e) {
      return acc;
    } else {
      acc.push(e);
      return acc;
    }
  });
}

function findNumbers(engine) {
  let numberRegions = engineFilter(engine, isDigit).toSorted(Coordinate$AdventOfCode.Compare.yx);
  let rowsWithNumber = rowsFromRegion(numberRegions);
  let groupedByRow = rowsWithNumber.map(y => numberRegions.filter(param => param[1] === y));
  return groupedByRow.map(__x => __x.toSorted(Coordinate$AdventOfCode.Compare.xy)).map(l => {
    l.push([
      0,
      0
    ]);
    return Stdlib__Array.reduce(l, [
        [],
        []
      ], (param, param$1) => {
        let y = param$1[1];
        let x = param$1[0];
        let buf = param[1];
        let parsed = param[0];
        let last = buf.at(buf.length - 1 | 0);
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
      })[0];
  }).flat();
}

function findSymbols(__x) {
  return engineFilter(__x, isSymbol);
}

function findStars(__x) {
  return engineFilter(__x, isStar);
}

function touchedDigit(engine, symbols) {
  return symbols.flatMap(__x => getNeighborsIf(__x, __x => isElemDigit(engine, __x)));
}

function isNumberTouched(number, symTouched) {
  return symTouched.some(sPos => number.some(nPos => Primitive_object.equal(nPos, sPos)));
}

function getNumber(engine, number) {
  return Stdlib__Array.reduce(number.map(n => {
    let match = Array2D$AdventOfCode.get(engine, n);
    if (typeof match === "object" && match.TAG !== "Symbol") {
      return match._0;
    } else {
      return Pervasives.failwith("expected digit");
    }
  }), 0, (acc, d) => Math.imul(acc, 10) + d | 0);
}

function findNumbersTouched(engine, numbers, symTouched) {
  return Stdlib__Array.filterMap(numbers, num => {
    if (isNumberTouched(num, symTouched)) {
      return getNumber(engine, num);
    }
    
  });
}

function part1(engine) {
  let symbols = engineFilter(engine, isSymbol);
  let numbers = findNumbers(engine);
  let symTouched = touchedDigit(engine, symbols);
  return Utils$AdventOfCode.sumIntArray(findNumbersTouched(engine, numbers, symTouched));
}

function part2(engine) {
  let stars = engineFilter(engine, isStar);
  let numbers = findNumbers(engine);
  let starTouched = Stdlib__Array.filterMap(stars, s => {
    let t = touchedDigit(engine, [s]);
    if (t.length >= 2) {
      return t;
    }
    
  });
  return Utils$AdventOfCode.sumIntArray(Stdlib__Array.filterMap(starTouched, x => {
    let touchedNums = findNumbersTouched(engine, numbers, x);
    if (touchedNums.length === 2) {
      return Utils$AdventOfCode.mulIntArray(touchedNums);
    }
    
  }));
}

function parse(data) {
  return Utils$AdventOfCode.splitNewline(data).map(Utils$AdventOfCode.compose(prim => prim.trim(), Utils$AdventOfCode.splitChars));
}

function solvePart1(data) {
  return part1(Array2D$AdventOfCode.map(parse(data), makeElem));
}

function solvePart2(data) {
  return part2(Array2D$AdventOfCode.map(parse(data), makeElem));
}

export {
  log,
  log2,
  makeElem,
  isDigit,
  isSymbol,
  isStar,
  getNeighborsIf,
  isElemDigit,
  makeEngine,
  engineFilter,
  rowsFromRegion,
  findNumbers,
  findSymbols,
  findStars,
  touchedDigit,
  isNumberTouched,
  getNumber,
  findNumbersTouched,
  part1,
  part2,
  parse,
  solvePart1,
  solvePart2,
}
/* Stdlib__Int Not a pure module */
