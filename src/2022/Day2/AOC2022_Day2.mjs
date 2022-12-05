// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
}

function player2Result(round) {
  switch (round._0) {
    case /* Rock */0 :
        switch (round._1) {
          case /* Rock */0 :
              return /* Draw */1;
          case /* Paper */1 :
              return /* Win */2;
          case /* Scissors */2 :
              return /* Loss */0;
          
        }
    case /* Paper */1 :
        switch (round._1) {
          case /* Rock */0 :
              return /* Loss */0;
          case /* Paper */1 :
              return /* Draw */1;
          case /* Scissors */2 :
              return /* Win */2;
          
        }
    case /* Scissors */2 :
        switch (round._1) {
          case /* Rock */0 :
              return /* Win */2;
          case /* Paper */1 :
              return /* Loss */0;
          case /* Scissors */2 :
              return /* Draw */1;
          
        }
    
  }
}

function scoreShape(s) {
  return 1 + (s + 1 | 0) | 0;
}

function scoreResult(r) {
  var tmp;
  switch (r) {
    case /* Loss */0 :
        tmp = 0;
        break;
    case /* Draw */1 :
        tmp = 3;
        break;
    case /* Win */2 :
        tmp = 6;
        break;
    
  }
  return Math.imul(3, tmp);
}

function scoreRound(r) {
  return scoreResult(player2Result(r)) + scoreShape(r._1) | 0;
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
                return prim.trim();
              }));
}

function solvePart1(data) {
  return 1;
}

function solvePart2(data) {
  return 2;
}

export {
  log ,
  player2Result ,
  scoreShape ,
  scoreResult ,
  scoreRound ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* No side effect */
