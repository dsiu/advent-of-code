open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

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
    t.prog->Array.getExn(pos)
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

  let setNounVerb = (t, noun, verb) => {
    t->saveVal(1, noun)->saveVal(2, verb)
  }

  let make = xs => {prog: xs, pc: 0}

  let executeWithNounVerb = (prog, noun, verb) => {
    let result = prog->setNounVerb(noun, verb)->execute
    result.prog[0]->Option.getExn
  }
}

let part1 = xs => {
  open IntCode
  executeWithNounVerb(make(xs), 12, 2)
}

let part2 = xs => {
  open IntCode
  //  let result = ref(None)
  //
  //  for noun in 0 to 99 {
  //    for verb in 0 to 99 {
  //      if executeWithNounVerb(make(xs->Array.copy), noun, verb) == 19690720 {
  //        result := Some(noun * 100 + verb)
  //      }
  //    }
  //  }

  let {combinationIf2} = module(Stdlib.Array)

  combinationIf2(Array.range(0, 99), Array.range(0, 99), (. noun, verb) => {
    executeWithNounVerb(make(xs->Array.copy), noun, verb) == 19690720
      ? Some(noun * 100 + verb)
      : None
  })->Array.getExn(0)
}

let solvePart1 = data => {
  let prog = data->parse
  part1(prog)
}

let solvePart2 = data => {
  let prog = data->parse
  part2(prog)
}
