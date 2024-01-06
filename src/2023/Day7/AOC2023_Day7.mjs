// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Stdlib__Result from "@dsiu/rescript-stdlib-fp/src/Stdlib__Result.mjs";
import * as ReludeParse_Parser from "relude-parse/src/ReludeParse_Parser.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var justSpace = Curry._1(ReludeParse_Parser.$$void, ReludeParse_Parser.many(ReludeParse_Parser.str(" ")));

function handC(a, b) {
  var cards = Stdlib__List.toArray(a);
  return {
          TAG: "Hand",
          _0: cards,
          _1: b
        };
}

var cardP = Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$, "Two", ReludeParse_Parser.str("2")), Curry._2(ReludeParse_Parser.Infix.$less$, "Three", ReludeParse_Parser.str("3"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Four", ReludeParse_Parser.str("4"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Five", ReludeParse_Parser.str("5"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Six", ReludeParse_Parser.str("6"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Seven", ReludeParse_Parser.str("7"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Eight", ReludeParse_Parser.str("8"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Nine", ReludeParse_Parser.str("9"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Ten", ReludeParse_Parser.str("T"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Jack", ReludeParse_Parser.str("J"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Queen", ReludeParse_Parser.str("Q"))), Curry._2(ReludeParse_Parser.Infix.$less$, "King", ReludeParse_Parser.str("K"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Ace", ReludeParse_Parser.str("A")));

var handP = Curry._2(ReludeParse_Parser.Infix.$less$star$great, Curry._2(ReludeParse_Parser.Infix.$less$$great, handC, Curry._2(ReludeParse_Parser.Infix.$star$great, justSpace, Curry._2(ReludeParse_Parser.Infix.$less$star, ReludeParse_Parser.many(cardP), justSpace))), ReludeParse_Parser.anyInt);

var handsP = ReludeParse_Parser.sepBy(ReludeParse_Parser.eol, handP);

var cardP$1 = Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$pipe$great, Curry._2(ReludeParse_Parser.Infix.$less$, "Two", ReludeParse_Parser.str("2")), Curry._2(ReludeParse_Parser.Infix.$less$, "Three", ReludeParse_Parser.str("3"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Four", ReludeParse_Parser.str("4"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Five", ReludeParse_Parser.str("5"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Six", ReludeParse_Parser.str("6"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Seven", ReludeParse_Parser.str("7"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Eight", ReludeParse_Parser.str("8"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Nine", ReludeParse_Parser.str("9"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Ten", ReludeParse_Parser.str("T"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Jack", ReludeParse_Parser.str("J"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Queen", ReludeParse_Parser.str("Q"))), Curry._2(ReludeParse_Parser.Infix.$less$, "King", ReludeParse_Parser.str("K"))), Curry._2(ReludeParse_Parser.Infix.$less$, "Ace", ReludeParse_Parser.str("A")));

function run(str) {
  return Stdlib__List.toArray(Stdlib__Result.getExn(ReludeParse_Parser.runParser(str, handsP)));
}

var HandsParser = {
  P: undefined,
  justSpace: justSpace,
  handC: handC,
  handP: handP,
  handsP: handsP,
  cardP: cardP$1,
  run: run
};

function solvePart1(data) {
  var prim = run(data);
  console.log(prim);
  return 1;
}

function solvePart2(data) {
  return 2;
}

export {
  log ,
  log2 ,
  HandsParser ,
  solvePart1 ,
  solvePart2 ,
}
/* justSpace Not a pure module */
