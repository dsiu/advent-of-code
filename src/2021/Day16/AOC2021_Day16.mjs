// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Js__Int from "rescript-js/src/Js__Int.mjs";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Res_parser from "@resinfo/parser/src/res_parser.mjs";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_Result from "rescript/lib/es6/belt_Result.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
  
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
  
}

function concatStringList(chars) {
  if (chars) {
    return chars.hd + concatStringList(chars.tl);
  } else {
    return "";
  }
}

function charToString(c) {
  return String.fromCharCode(c);
}

function stringifyCharList(chars) {
  return concatStringList(Belt_List.map(chars, charToString));
}

function binCharArrayToStr(xs) {
  return Belt_Array.reduce(xs, "", (function (a, v) {
                return a + String.fromCharCode(v);
              }));
}

function binCharArrayToInt(xs) {
  return parseInt(binCharArrayToStr(xs), 2);
}

var binDigit = Res_parser.satisfy(function (c) {
      if (c === /* '0' */48) {
        return true;
      } else {
        return c === /* '1' */49;
      }
    });

var threeBinDigits = Res_parser.andThen(Res_parser.andThen(binDigit, binDigit), binDigit);

function threeBinDigitsMap(f) {
  return Res_parser.map(threeBinDigits, (function (param) {
                var match = param[0];
                return Curry._1(f, [
                            match[0],
                            match[1],
                            param[1]
                          ]);
              }));
}

var threeBinDigitsToInt = threeBinDigitsMap(binCharArrayToInt);

var threeBinDigitsToString = threeBinDigitsMap(binCharArrayToStr);

var fourBinDigits = Res_parser.andThen(Res_parser.andThen(Res_parser.andThen(binDigit, binDigit), binDigit), binDigit);

function fourBinDigitsMap(f) {
  return Res_parser.map(fourBinDigits, (function (param) {
                var match = param[0];
                var match$1 = match[0];
                return Curry._1(f, [
                            match$1[0],
                            match$1[1],
                            match[1],
                            param[1]
                          ]);
              }));
}

var fourBinDigitsToInt = fourBinDigitsMap(binCharArrayToInt);

var fourBinDigitsToStr = fourBinDigitsMap(binCharArrayToStr);

var version = Res_parser.map(threeBinDigitsToInt, (function (x) {
        return /* Version */{
                _0: x
              };
      }));

var typeId = Res_parser.map(threeBinDigitsToInt, (function (x) {
        return /* TypeID */{
                _0: x
              };
      }));

var oneAndFourBit = Res_parser.andThen(Res_parser.$$char(/* '1' */49), fourBinDigitsToStr);

var zeroAndFourBit = Res_parser.andThen(Res_parser.$$char(/* '0' */48), fourBinDigitsToStr);

var literal = Res_parser.andThen(Res_parser.many(oneAndFourBit), zeroAndFourBit);

var payload = Res_parser.map(literal, (function (param) {
        return /* Payload_Literal */{
                _0: parseInt(Belt_List.reduce(param[0], "", (function (a, param) {
                            return a + param[1];
                          })) + param[1][1], 2)
              };
      }));

var packet = Res_parser.map(Res_parser.andThen(Res_parser.andThen(version, typeId), payload), (function (param) {
        var match = param[0];
        return /* Packet */{
                _0: match[0],
                _1: match[1],
                _2: param[1]
              };
      }));

function dumpPacket(param) {
  return "ver = " + param._0._0 + " | typeId = " + param._1._0 + " | payload = " + param._2._0;
}

function parse(s) {
  return Res_parser.run(packet, s);
}

function hexStrToBinStr(s) {
  return Belt_Option.flatMap(Js__Int.fromString(16, s), (function (x) {
                return x.toString(2);
              }));
}

var Packet_M = {
  binDigit: binDigit,
  threeBinDigits: threeBinDigits,
  threeBinDigitsMap: threeBinDigitsMap,
  threeBinDigitsToInt: threeBinDigitsToInt,
  threeBinDigitsToString: threeBinDigitsToString,
  fourBinDigits: fourBinDigits,
  fourBinDigitsMap: fourBinDigitsMap,
  fourBinDigitsToInt: fourBinDigitsToInt,
  fourBinDigitsToStr: fourBinDigitsToStr,
  version: version,
  typeId: typeId,
  payload: payload,
  packet: packet,
  dumpPacket: dumpPacket,
  parse: parse,
  hexStrToBinStr: hexStrToBinStr
};

function parse$1(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
                return prim.trim();
              }));
}

function solvePart1(data) {
  var r = Res_parser.run(packet, "110100101111111000101000");
  var prim = Belt_Result.isOk(r);
  console.log(prim);
  var prim$1 = dumpPacket(Belt_Result.getExn(r)[0]);
  console.log(prim$1);
  var prim$2 = Belt_Result.getExn(r)[1];
  console.log(prim$2);
  return 1;
}

function solvePart2(data) {
  return 2;
}

var P;

var Rjs;

export {
  log ,
  log2 ,
  P ,
  Rjs ,
  concatStringList ,
  charToString ,
  stringifyCharList ,
  binCharArrayToStr ,
  binCharArrayToInt ,
  Packet_M ,
  parse$1 as parse,
  solvePart1 ,
  solvePart2 ,
  
}
/* binDigit Not a pure module */