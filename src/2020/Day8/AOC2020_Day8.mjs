// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Int from "rescript/lib/es6/Belt_Int.js";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Belt_SetInt from "rescript/lib/es6/Belt_SetInt.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

function log(prim) {
  console.log(prim);
}

function opcode(t) {
  return t.opcode;
}

function arg(t) {
  return t.arg;
}

function setOpcode(t, opcode) {
  return {
    opcode: opcode,
    arg: t.arg
  };
}

let Invalid = /* @__PURE__ */Primitive_exceptions.create("AOC2020_Day8-AdventOfCode.Instruction.Invalid");

function make(op, arg) {
  let match = Belt_Int.fromString(arg);
  switch (op) {
    case "acc" :
      if (match !== undefined) {
        return {
          opcode: "ACC",
          arg: match
        };
      }
      break;
    case "jmp" :
      if (match !== undefined) {
        return {
          opcode: "JMP",
          arg: match
        };
      }
      break;
    case "nop" :
      if (match !== undefined) {
        return {
          opcode: "NOP",
          arg: match
        };
      }
      break;
  }
  throw {
    RE_EXN_ID: Invalid,
    _1: "Invalid Opcodd:" + op,
    Error: new Error()
  };
}

let Instruction = {
  opcode: opcode,
  arg: arg,
  setOpcode: setOpcode,
  Invalid: Invalid,
  make: make
};

function program(t) {
  return t.program;
}

function pc(t) {
  return t.pc;
}

function accumulator(t) {
  return t.accumulator;
}

function executed(t) {
  return t.executed;
}

function state(t) {
  return t.state;
}

function getCurrentInstruction(t) {
  return Belt_Array.getExn(t.program, t.pc);
}

function make$1(program) {
  return {
    program: program,
    pc: 0,
    accumulator: 0,
    executed: undefined,
    state: "EXECUTE"
  };
}

function hasExecuted(t) {
  return Belt_SetInt.has(t.executed, t.pc);
}

function markExecuted(t) {
  return {
    program: t.program,
    pc: t.pc,
    accumulator: t.accumulator,
    executed: Belt_SetInt.add(t.executed, t.pc),
    state: t.state
  };
}

function shouldEnd(t) {
  if (t.pc >= t.program.length) {
    return true;
  } else {
    return t.pc < 0;
  }
}

function executeInstruction(t) {
  let instr = getCurrentInstruction(t);
  let match = instr.opcode;
  if (match === "JMP") {
    let init = markExecuted(t);
    return {
      program: init.program,
      pc: t.pc + instr.arg | 0,
      accumulator: init.accumulator,
      executed: init.executed,
      state: init.state
    };
  }
  if (match === "NOP") {
    let init$1 = markExecuted(t);
    return {
      program: init$1.program,
      pc: t.pc + 1 | 0,
      accumulator: init$1.accumulator,
      executed: init$1.executed,
      state: init$1.state
    };
  }
  let init$2 = markExecuted(t);
  return {
    program: init$2.program,
    pc: t.pc + 1 | 0,
    accumulator: t.accumulator + instr.arg | 0,
    executed: init$2.executed,
    state: init$2.state
  };
}

function execute(_t) {
  while (true) {
    let t = _t;
    let match = hasExecuted(t);
    let match$1 = shouldEnd(t);
    if (match) {
      return {
        program: t.program,
        pc: t.pc,
        accumulator: t.accumulator,
        executed: t.executed,
        state: "INF"
      };
    }
    if (match$1) {
      return {
        program: t.program,
        pc: t.pc,
        accumulator: t.accumulator,
        executed: t.executed,
        state: "DONE"
      };
    }
    _t = executeInstruction(t);
    continue;
  };
}

let Machine = {
  program: program,
  pc: pc,
  accumulator: accumulator,
  executed: executed,
  state: state,
  getCurrentInstruction: getCurrentInstruction,
  make: make$1,
  hasExecuted: hasExecuted,
  markExecuted: markExecuted,
  shouldEnd: shouldEnd,
  executeInstruction: executeInstruction,
  execute: execute
};

function parseLine(l) {
  let x = l.trim().split(" ");
  let op = Belt_Array.getExn(x, 0);
  let arg = Belt_Array.getExn(x, 1);
  return make(op, arg);
}

function parse(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), parseLine);
}

function solvePart1(data) {
  let m = make$1(Belt_Array.map(Utils$AdventOfCode.splitNewline(data), parseLine));
  return execute(m).accumulator;
}

function genPatched(p) {
  return Belt_Array.reduceWithIndex(p, [], (a, x, i) => {
    let match = x.opcode;
    if (match === "JMP") {
      let patched = p.slice(0);
      Belt_Array.set(patched, i, setOpcode(x, "NOP"));
      return Belt_Array.concat(a, [patched]);
    }
    if (match !== "NOP") {
      return a;
    }
    let patched$1 = p.slice(0);
    Belt_Array.set(patched$1, i, setOpcode(x, "JMP"));
    return Belt_Array.concat(a, [patched$1]);
  });
}

function solvePart2(data) {
  let p = Belt_Array.map(Utils$AdventOfCode.splitNewline(data), parseLine);
  let patched = genPatched(p);
  let rans = Belt_Array.map(patched, x => execute(make$1(x)));
  let res = Belt_Array.keep(rans, x => {
    let match = x.state;
    return match === "DONE";
  });
  return Belt_Option.getExn(Belt_Array.get(res, 0)).accumulator;
}

export {
  log,
  Instruction,
  Machine,
  parseLine,
  parse,
  solvePart1,
  genPatched,
  solvePart2,
}
/* Utils-AdventOfCode Not a pure module */
