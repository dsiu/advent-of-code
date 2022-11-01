open Belt
open Utils
let log = Js.Console.log

let parse = data => data->Js.String2.split(_, ",")->Array.map(intFromStringExn)

module IntCode = {
  type t = {
    prog: array<int>,
    pc: int,
  }

  type instr = {
    op: int,
    arg1: int,
    arg2: int,
    out: int,
  }

  let getVal = (t, pos) => {
    t.prog[pos]->Option.getExn
  }

  let saveVal = (t, pos, v) => {
    t.prog->Array.setExn(pos, v)
    t
  }

  exception InvalidOpCode(int)

  let executeInstr = (t, instr) => {
    switch instr.op {
    | 1 => saveVal(t, instr.out, getVal(t, instr.arg1) + getVal(t, instr.arg2))
    | 2 => saveVal(t, instr.out, getVal(t, instr.arg1) * getVal(t, instr.arg2))
    | _ => raise(InvalidOpCode(instr.op))
    }
  }

  let getInstr = t => {
    let op = getVal(t, t.pc)
    let arg1 = getVal(t, t.pc + 1)
    let arg2 = getVal(t, t.pc + 2)
    let out = getVal(t, t.pc + 3)

    switch op {
    | 1
    | 2 =>
      Some({op, arg1, arg2, out})
    | 99 => None
    | _ => raise(InvalidOpCode(op))
    }
  }

  let rec execute = t => {
    let i = t->getInstr
    switch i {
    | Some(i) => {
        let t = executeInstr(t, i)
        execute({...t, pc: t.pc + 4})
      }

    | None => t
    }
  }

  let fix1202Alarm = t => {
    t->saveVal(1, 12)->saveVal(2, 2)
  }

  let make = xs => {prog: xs, pc: 0}
}

let part1 = prog => {
  open IntCode
  let result = prog->fix1202Alarm->execute
  result.prog[0]->Option.getExn
}

let solvePart1 = data => {
  let prog = data->parse->IntCode.make
  part1(prog)
}

let solvePart2 = data => {
  data->ignore
  2
}
