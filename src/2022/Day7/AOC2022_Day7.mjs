// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Res_parser from "@resinfo/parser/src/res_parser.mjs";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Stdlib__Array from "@dsiu/rescript-stdlib-fp/src/Stdlib__Array.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as ResParser_Utils$AdventOfCode from "../../ResParser_Utils.mjs";

function log(prim) {
  console.log(prim);
}

function parsedObjectToString(obj) {
  if (typeof obj !== "object") {
    return "LS";
  }
  switch (obj.TAG) {
    case "CD" :
        return "CD " + obj._0;
    case "PDirectory" :
        return "PDirectory " + obj._0;
    case "PFile" :
        return "PFile " + String(obj._0) + " " + obj._1;
    
  }
}

var cmdCD = Res_parser.map(Res_parser.andThen(Res_parser.string("$ cd "), ResParser_Utils$AdventOfCode.string), (function (param) {
        return {
                TAG: "CD",
                _0: param[1]
              };
      }));

var cmdLS = Res_parser.map(Res_parser.string("$ ls"), (function (param) {
        return "LS";
      }));

var outputDir = Res_parser.map(Res_parser.andThen(Res_parser.string("dir "), ResParser_Utils$AdventOfCode.string), (function (param) {
        return {
                TAG: "PDirectory",
                _0: param[1]
              };
      }));

var outputFile = Res_parser.map(Res_parser.andThen(Res_parser.andThen(ResParser_Utils$AdventOfCode.digits, ResParser_Utils$AdventOfCode.manyWhitespace), ResParser_Utils$AdventOfCode.string), (function (param) {
        return {
                TAG: "PFile",
                _0: Caml_format.int_of_string(param[0][0]),
                _1: param[1]
              };
      }));

var parser = Res_parser.choice([
      cmdCD,
      cmdLS,
      outputDir,
      outputFile
    ]);

var result = Res_parser.run(parser, "$ cd /");

function run(__x) {
  return Res_parser.run(parser, __x);
}

var CmdParser = {
  cmdCD: cmdCD,
  cmdLS: cmdLS,
  outputDir: outputDir,
  outputFile: outputFile,
  parser: parser,
  result: result,
  run: run
};

function parse(data) {
  return Stdlib__Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
                return prim.trim();
              }));
}

function solvePart1(data) {
  var prim = Stdlib__Array.filterMap(Stdlib__Array.map(parse(data), run), (function (result) {
          if (result.TAG === "Ok") {
            return parsedObjectToString(result._0[0]);
          }
          
        }));
  console.log(prim);
  return 1;
}

function solvePart2(data) {
  return 2;
}

var A;

var M;

export {
  log ,
  A ,
  parsedObjectToString ,
  CmdParser ,
  M ,
  parse ,
  solvePart1 ,
  solvePart2 ,
}
/* cmdCD Not a pure module */
