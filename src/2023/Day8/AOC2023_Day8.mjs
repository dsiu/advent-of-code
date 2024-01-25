// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Stdlib__Int from "@dsiu/rescript-stdlib-fp/src/Stdlib__Int.mjs";
import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Stdlib__Math from "@dsiu/rescript-stdlib-fp/src/Stdlib__Math.mjs";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Stdlib__Option from "@dsiu/rescript-stdlib-fp/src/Stdlib__Option.mjs";
import * as Stdlib__Result from "@dsiu/rescript-stdlib-fp/src/Stdlib__Result.mjs";
import * as Stdlib__String from "@dsiu/rescript-stdlib-fp/src/Stdlib__String.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";
import * as Stdlib__Ordering from "@dsiu/rescript-stdlib-fp/src/Stdlib__Ordering.mjs";
import * as ReludeParse_Parser from "relude-parse/src/ReludeParse_Parser.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function compare(param, param$1) {
  var h = Stdlib__String.compare(param.here, param$1.here);
  if (Stdlib__Ordering.isEqual(h)) {
    return Stdlib__Int.compare(param.steps, param$1.steps);
  } else {
    return h;
  }
}

var State = {
  compare: compare
};

function getDirection(directions, steps) {
  return Stdlib__Array.getUnsafe(directions, Caml_int32.mod_(steps, directions.length));
}

function isGoal(param) {
  return Stdlib__String.last(param.here) === "Z";
}

function step(desert, param, direction) {
  var steps = param.steps;
  var match = Stdlib__Option.getExn(desert.get(param.here));
  if (direction === "L") {
    return {
            here: match._0,
            steps: steps + 1 | 0
          };
  } else {
    return {
            here: match._1,
            steps: steps + 1 | 0
          };
  }
}

function walk(desert, directions, _state) {
  while(true) {
    var state = _state;
    if (isGoal(state)) {
      return state;
    }
    _state = step(desert, state, getDirection(directions, state.steps));
    continue ;
  };
}

function part1(param) {
  return walk(param[1], param[0], {
              here: "AAA",
              steps: 0
            }).steps;
}

function part2(param) {
  var desert = param[1];
  var directions = param[0];
  return Stdlib__Array.foldl1(Array.from(desert.keys()).filter(function (param) {
                      return Stdlib__Function.compose3(Stdlib__String.last, (function (param) {
                                    return Stdlib__String.compare("A", param);
                                  }), Stdlib__Ordering.isEqual, param);
                    }).map(function (s) {
                    return walk(desert, directions, {
                                here: s,
                                steps: 0
                              }).steps;
                  }).map(function (prim) {
                  return BigInt(prim);
                }), Stdlib__Math.lcmBigInt);
}

var justSpace = Curry._1(ReludeParse_Parser.$$void, ReludeParse_Parser.many(ReludeParse_Parser.str(" ")));

function mkNode(a, b) {
  return {
          TAG: "Node",
          _0: a,
          _1: b
        };
}

function mkDesertLine(a, b) {
  return [
          a,
          b
        ];
}

var nameP = ReludeParse_Parser.$less$hash$great(ReludeParse_Parser.many(ReludeParse_Parser.anyAlphaOrDigit), (function (l) {
        return Stdlib__List.toArray(l).join("");
      }));

var nodeP = Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$$great, mkNode, Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$star$great, ReludeParse_Parser.str("("), nameP), ReludeParse_Parser.str(", "))), Curry._2(ReludeParse_Parser.Infix.$less$star, nameP, ReludeParse_Parser.str(")")));

var desertLineP = Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$$great, mkDesertLine, Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$star$great, justSpace, nameP), ReludeParse_Parser.str(" = "))), nodeP);

function mkDesert(a) {
  return new Map(Stdlib__List.toArray(a));
}

var desertP = Curry._2(ReludeParse_Parser.Infix.$less$$great, mkDesert, ReludeParse_Parser.sepBy(ReludeParse_Parser.eol, desertLineP));

var directionP = Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$, "L", ReludeParse_Parser.str("L")), Curry._2(ReludeParse_Parser.Infix.$less$, "R", ReludeParse_Parser.str("R")));

function mkProblem(a, b) {
  return [
          Stdlib__List.toArray(a),
          b
        ];
}

var problemP = Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$$great, mkProblem, Curry._2(ReludeParse_Parser.Infix.$less$star, ReludeParse_Parser.many(directionP), ReludeParse_Parser.many1(ReludeParse_Parser.eol))), desertP);

function run(data) {
  return Stdlib__Result.getExn(ReludeParse_Parser.runParser(data, problemP));
}

var ProblemParser = {
  P: undefined,
  justSpace: justSpace,
  debug: ReludeParse_Parser.tapLog,
  mkNode: mkNode,
  mkDesertLine: mkDesertLine,
  nameP: nameP,
  nodeP: nodeP,
  desertLineP: desertLineP,
  mkDesert: mkDesert,
  desertP: desertP,
  directionP: directionP,
  mkProblem: mkProblem,
  problemP: problemP,
  run: run
};

function solvePart1(data) {
  return part1(Stdlib__Result.getExn(ReludeParse_Parser.runParser(data, problemP)));
}

function solvePart2(data) {
  return part2(Stdlib__Result.getExn(ReludeParse_Parser.runParser(data, problemP)));
}

export {
  log ,
  log2 ,
  State ,
  getDirection ,
  isGoal ,
  step ,
  walk ,
  part1 ,
  part2 ,
  ProblemParser ,
  solvePart1 ,
  solvePart2 ,
}
/* justSpace Not a pure module */
