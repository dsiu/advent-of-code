// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

var InvalidMove = /* @__PURE__ */Caml_exceptions.create("AOC2021_Day2-AdventOfCode.Move.InvalidMove");

function make(s, n) {
  switch (s) {
    case "down" :
        return {
                TAG: "Down",
                _0: n
              };
    case "forward" :
        return {
                TAG: "Forward",
                _0: n
              };
    case "up" :
        return {
                TAG: "Up",
                _0: n
              };
    default:
      throw {
            RE_EXN_ID: InvalidMove,
            _1: s,
            Error: new Error()
          };
  }
}

var Move = {
  InvalidMove: InvalidMove,
  make: make
};

var make$1 = {
  h: 0,
  d: 0,
  aim: 0
};

function move(t, move$1) {
  switch (move$1.TAG) {
    case "Forward" :
        return {
                h: t.h + move$1._0 | 0,
                d: t.d,
                aim: t.aim
              };
    case "Down" :
        return {
                h: t.h,
                d: t.d + move$1._0 | 0,
                aim: t.aim
              };
    case "Up" :
        return {
                h: t.h,
                d: t.d - move$1._0 | 0,
                aim: t.aim
              };
    
  }
}

function moveWithAim(t, move) {
  switch (move.TAG) {
    case "Forward" :
        var n = move._0;
        return {
                h: t.h + n | 0,
                d: t.d + Math.imul(t.aim, n) | 0,
                aim: t.aim
              };
    case "Down" :
        return {
                h: t.h,
                d: t.d,
                aim: t.aim + move._0 | 0
              };
    case "Up" :
        return {
                h: t.h,
                d: t.d,
                aim: t.aim - move._0 | 0
              };
    
  }
}

var Submarine = {
  make: make$1,
  move: move,
  moveWithAim: moveWithAim
};

function run(xs, f) {
  return Belt_Array.reduce(xs, make$1, (function (a, x) {
                return f(a, x);
              }));
}

function runPart1(__x) {
  return run(__x, move);
}

function runPart2(__x) {
  return run(__x, moveWithAim);
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                var s = x.trim().split(" ");
                return make(Belt_Option.getExn(Belt_Array.get(s, 0)), Utils$AdventOfCode.intFromStringExn(Belt_Option.getExn(Belt_Array.get(s, 1))));
              }));
}

function answer(t) {
  return Math.imul(t.h, t.d);
}

function solvePart1(data) {
  return answer(runPart1(parse(data)));
}

function solvePart2(data) {
  return answer(runPart2(parse(data)));
}

export {
  log ,
  Move ,
  Submarine ,
  run ,
  runPart1 ,
  runPart2 ,
  parse ,
  answer ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
