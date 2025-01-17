// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int64 from "rescript/lib/es6/int64.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_format from "rescript/lib/es6/caml_format.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Belt_MutableMapString from "rescript/lib/es6/belt_MutableMapString.js";
import * as Powerset$AdventOfCode from "../../Powerset.mjs";

function onlyXto1(c) {
  if (c === "X") {
    return "1";
  } else {
    return "0";
  }
}

function only1to1(c) {
  if (c === "1") {
    return "1";
  } else {
    return "0";
  }
}

function only0to1(c) {
  if (c === "0") {
    return "1";
  } else {
    return "0";
  }
}

function makeMask(m, f) {
  return Belt_Array.map(Utils$AdventOfCode.splitChars(m), f).join("");
}

function toString(param) {
  return "mask: " + param.mask + "\n" + ("mask_x: " + Int64.to_string(param.mask_x) + "\n") + ("mask_x_str: " + param.mask_x_str) + ("mask_one: " + Int64.to_string(param.mask_one) + "\n") + ("mask_zero: " + Int64.to_string(param.mask_zero));
}

function makePassThurMask(__x) {
  return makeMask(__x, onlyXto1);
}

function makeOneMask(__x) {
  return makeMask(__x, only1to1);
}

function makeZeroMask(__x) {
  return makeMask(__x, only0to1);
}

function make(str) {
  return {
          mask: str.slice("mask = ".length),
          mask_x: Utils$AdventOfCode.int64FromBitString(makeMask(str, onlyXto1)),
          mask_x_str: makeMask(str, onlyXto1),
          mask_one: Utils$AdventOfCode.int64FromBitString(makeMask(str, only1to1)),
          mask_zero: Utils$AdventOfCode.int64FromBitString(makeMask(str, only0to1))
        };
}

var Mask = {
  onlyXto1: onlyXto1,
  only1to1: only1to1,
  only0to1: only0to1,
  makeMask: makeMask,
  toString: toString,
  makePassThurMask: makePassThurMask,
  makeOneMask: makeOneMask,
  makeZeroMask: makeZeroMask,
  make: make
};

function make$1(str) {
  var prog_line_re = /mem\[(\d+)\]\s*=\s*(\d+)/i;
  var parsed = Belt_Array.map(Belt_Option.getExn((function (__x) {
                return Caml_option.null_to_opt(__x.exec(str));
              })(prog_line_re)), (function (l) {
          return Belt_Option.getExn((l == null) ? undefined : Caml_option.some(l));
        }));
  return {
          address: Belt_Option.getExn(Belt_Option.map(Belt_Array.get(parsed, 1), Caml_format.int64_of_string)),
          value: Belt_Option.getExn(Belt_Option.map(Belt_Array.get(parsed, 2), Caml_format.int64_of_string))
        };
}

function toString$1(t) {
  return "address: " + Int64.to_string(t.address) + "\nvalue: " + Int64.to_string(t.value);
}

var Memory = {
  make: make$1,
  toString: toString$1
};

function instructionToString(i) {
  if (i.TAG === "MaskOp") {
    return "MaskOp(" + toString(i._0) + ")";
  } else {
    return "MemOp(" + toString$1(i._0) + ")";
  }
}

var InvalidInstruction = /* @__PURE__ */Caml_exceptions.create("AOC2020_Day14-AdventOfCode.Program.InvalidInstruction");

function parse(lines) {
  return Belt_Array.map(lines, (function (l) {
                var match = l.substring(0, 4);
                switch (match) {
                  case "mask" :
                      return {
                              TAG: "MaskOp",
                              _0: make(l)
                            };
                  case "mem[" :
                      return {
                              TAG: "MemOp",
                              _0: make$1(l)
                            };
                  default:
                    throw {
                          RE_EXN_ID: InvalidInstruction,
                          _1: l,
                          Error: new Error()
                        };
                }
              }));
}

function make$2(instructions) {
  return {
          instructions: parse(instructions),
          memory: Belt_MutableMapString.make()
        };
}

function decodeMemory(mask, mem_value) {
  return Caml_int64.and_(Caml_int64.or_(Caml_int64.and_(mask.mask_x, mem_value), mask.mask_one), Int64.lognot(mask.mask_zero));
}

function part1Algo(space, mask, mem) {
  Belt_MutableMapString.update(space, Int64.to_string(mem.address), (function (v) {
          return decodeMemory(mask, mem.value);
        }));
}

function bit1Index(m) {
  var xs = Utils$AdventOfCode.splitChars(m);
  var len = xs.length;
  return Belt_Array.reduceWithIndex(xs, [], (function (a, x, i) {
                if (x === "1") {
                  return Belt_Array.concat(a, [(len - 1 | 0) - i | 0]);
                } else {
                  return a;
                }
              }));
}

function decodeAddress(mask, mem_address) {
  var pos_mask = Int64.lognot(mask.mask_x);
  var base = Caml_int64.and_(Caml_int64.or_(mem_address, mask.mask_one), pos_mask);
  var pos = bit1Index(mask.mask_x_str);
  var all_pos = Powerset$AdventOfCode.powersetArray(pos);
  return Belt_Array.map(all_pos, (function (pos) {
                var m = Belt_Array.reduce(pos, Int64.zero, (function (acc, x) {
                        return Caml_int64.or_(acc, Caml_int64.lsl_(Int64.one, x));
                      }));
                return Int64.to_string(Caml_int64.or_(base, m));
              }));
}

function part2Algo(space, mask, mem) {
  var addresses = decodeAddress(mask, mem.address);
  Belt_Array.forEach(addresses, (function (addr) {
          Belt_MutableMapString.update(space, addr, (function (v) {
                  return mem.value;
                }));
        }));
}

function run(t, algo) {
  var cur_m = {
    contents: make("mask = 11110000XXXX")
  };
  Belt_Array.forEach(t.instructions, (function (x) {
          if (x.TAG === "MaskOp") {
            cur_m.contents = x._0;
            return ;
          } else {
            return algo(t.memory, cur_m.contents, x._0);
          }
        }));
  return t.memory;
}

function runPart1(__x) {
  return run(__x, part1Algo);
}

function runPart2(__x) {
  return run(__x, part2Algo);
}

function toString$2(t) {
  return "=== Program dump ===\n" + Utils$AdventOfCode.Printable.$$Array.toString(t.instructions, instructionToString) + "\n" + Utils$AdventOfCode.Printable.MutableMapString.Int64.toString(t.memory);
}

var Program = {
  Mask: Mask,
  Memory: Memory,
  instructionToString: instructionToString,
  InvalidInstruction: InvalidInstruction,
  parse: parse,
  make: make$2,
  decodeMemory: decodeMemory,
  part1Algo: part1Algo,
  bit1Index: bit1Index,
  decodeAddress: decodeAddress,
  part2Algo: part2Algo,
  run: run,
  runPart1: runPart1,
  runPart2: runPart2,
  toString: toString$2
};

function parse$1(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (x) {
                return x.trim();
              }));
}

function memoryToAnswer(__x) {
  return Belt_MutableMapString.reduce(__x, Caml_int64.zero, (function (a, k, v) {
                return Caml_int64.add(v, a);
              }));
}

function solvePart1(data) {
  var prog = make$2(parse$1(data));
  var result = run(prog, part1Algo);
  return memoryToAnswer(result);
}

function solvePart2(data) {
  var prog = make$2(parse$1(data));
  var result = run(prog, part2Algo);
  return memoryToAnswer(result);
}

var toInt64 = Utils$AdventOfCode.int64FromBitString;

export {
  toInt64 ,
  Program ,
  parse$1 as parse,
  memoryToAnswer ,
  solvePart1 ,
  solvePart2 ,
}
/* Utils-AdventOfCode Not a pure module */
