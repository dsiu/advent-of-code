// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Relude_Int from "relude/src/Relude_Int.mjs";
import * as Relude_Map from "relude/src/Relude_Map.mjs";
import * as Relude_List from "relude/src/Relude_List.mjs";
import * as Relude_Result from "relude/src/Relude_Result.mjs";
import * as Relude_Function from "relude/src/Relude_Function.mjs";
import * as ReludeParse_Parser from "relude-parse/src/ReludeParse_Parser.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function makeLiteral(i) {
  return /* Literal */{
          _0: i
        };
}

function showExpression(param) {
  var operand = param._1;
  return (
          param._0 ? " * " : " + "
        ) + (
          operand ? String(operand._0) : "old"
        );
}

function logExp(param) {
  return Relude_Function.compose(log, showExpression, param);
}

function makeExpression(op, operand) {
  return /* Expression */{
          _0: op,
          _1: operand
        };
}

var include = Relude_Map.WithOrd(Relude_Int.Ord);

var IntMap = include;

var mIdP = Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$star$great, ReludeParse_Parser.str("Monkey "), ReludeParse_Parser.anyInt), ReludeParse_Parser.str(":")), ReludeParse_Parser.eol);

var startingP = Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$star$great, ReludeParse_Parser.str("  Starting items: "), ReludeParse_Parser.sepBy(ReludeParse_Parser.str(", "), ReludeParse_Parser.anyInt)), ReludeParse_Parser.eol);

var opP = Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$, /* Plus */0, ReludeParse_Parser.str("+")), Curry._2(ReludeParse_Parser.Infix.$less$, /* Times */1, ReludeParse_Parser.str("*")));

var operandP = Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$$great, makeLiteral, ReludeParse_Parser.anyInt), Curry._2(ReludeParse_Parser.Infix.$less$, /* Old */0, ReludeParse_Parser.str("old")));

var expressionP = Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$$great, makeExpression, Curry._2(ReludeParse_Parser.Infix.$less$star, opP, ReludeParse_Parser.str(" "))), operandP);

var operatorP = Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$star$great, ReludeParse_Parser.str("  Operation: new = old "), expressionP), ReludeParse_Parser.eol);

var testP = Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$star$great, ReludeParse_Parser.str("  Test: divisible by "), ReludeParse_Parser.anyInt), ReludeParse_Parser.eol);

var trueTargetP = Curry._2(ReludeParse_Parser.Infix.$less$star, Curry._2(ReludeParse_Parser.Infix.$star$great, ReludeParse_Parser.str("    If true: throw to monkey "), ReludeParse_Parser.anyInt), ReludeParse_Parser.eol);

var falseTargetP = Curry._2(ReludeParse_Parser.Infix.$star$great, ReludeParse_Parser.str("    If false: throw to monkey "), ReludeParse_Parser.anyInt);

function mkMonkeyPair(mId, holding, operation, test, trueTarget, falseTarget) {
  return [
          mId,
          Relude_List.toArray(holding),
          showExpression(operation),
          test,
          trueTarget,
          falseTarget
        ];
}

var monkeyP = Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$$great, mkMonkeyPair, mIdP), startingP), operatorP), testP), trueTargetP), falseTargetP);

function makeMonkeyMaps(monkeys) {
  return monkeys;
}

var monkeysP = Curry._2(ReludeParse_Parser.Infix.$less$$great, makeMonkeyMaps, ReludeParse_Parser.sepBy(Curry._2(ReludeParse_Parser.Infix.$less$star, ReludeParse_Parser.eol, ReludeParse_Parser.eol), monkeyP));

function parse(s) {
  return ReludeParse_Parser.runParser(s, monkeysP);
}

var MonkeyParser = {
  P: undefined,
  mIdP: mIdP,
  startingP: startingP,
  operatorP: operatorP,
  testP: testP,
  trueTargetP: trueTargetP,
  falseTargetP: falseTargetP,
  monkeyP: monkeyP,
  monkeysP: monkeysP,
  parse: parse
};

function solvePart1(data) {
  var result = ReludeParse_Parser.runParser(data, monkeysP);
  Relude_Result.tap((function (x) {
          var prim = Relude_List.toArray(x);
          console.log(prim);
        }), result);
  return 1;
}

function solvePart2(data) {
  return 2;
}

var S;

var A;

var L;

var O;

var R;

var F;

var compose = Relude_Function.compose;

export {
  log ,
  log2 ,
  S ,
  A ,
  L ,
  O ,
  R ,
  F ,
  compose ,
  makeLiteral ,
  showExpression ,
  logExp ,
  makeExpression ,
  IntMap ,
  MonkeyParser ,
  solvePart1 ,
  solvePart2 ,
}
/* include Not a pure module */
