open Belt
open Utils
let log = Js.Console.log

module Instruction = {
  type opcode = [#NOP | #ACC | #JMP]
  type arg = int

  type t = {
    opcode: opcode,
    arg: arg,
  }

  let opcode = t => t.opcode
  let arg = t => t.arg

  let setOpcode = (t, opcode) => {...t, opcode: opcode}

  exception Invalid(string)

  let make = (op, arg) => {
    switch (op, arg->Int.fromString) {
    | ("nop", Some(i)) => {opcode: #NOP, arg: i}
    | ("acc", Some(i)) => {opcode: #ACC, arg: i}
    | ("jmp", Some(i)) => {opcode: #JMP, arg: i}
    | (o, _) => raise(Invalid("Invalid Opcodd:" ++ o))
    }
  }
}

module Machine = {
  type program = array<Instruction.t>
  type state = [#DONE | #INF | #EXECUTE]

  type t = {
    program: program, // ordered array of instruction
    pc: int, // program counter
    accumulator: int,
    executed: Set.Int.t, // tracking which instruction has been executed
    state: state,
  }

  let program = t => t.program
  let pc = t => t.pc
  let accumulator = t => t.accumulator
  let executed = t => t.executed
  let state = t => t.state

  let getCurrentInstruction = t => t->program->Array.get(t->pc)->Option.getExn

  let make = program => {
    program: program,
    pc: 0,
    accumulator: 0,
    executed: Set.Int.empty,
    state: #EXECUTE,
  }

  let hasExecuted = t => t->executed->Set.Int.has(t->pc)
  let markExecuted = t => {...t, executed: t->executed->Set.Int.add(t->pc)}
  let shouldEnd = t => t->pc >= t->program->Array.size || t->pc < 0

  let executeInstruction = t => {
    let instr = t->getCurrentInstruction
    //    t->pc->log
    //    instr->log
    switch instr->Instruction.opcode {
    | #NOP => {...t->markExecuted, pc: t->pc + 1}
    | #JMP => {...t->markExecuted, pc: t->pc + instr->Instruction.arg}
    | #ACC => {
        ...t->markExecuted,
        accumulator: t->accumulator + instr->Instruction.arg,
        pc: t->pc + 1,
      }
    }
  }

  let rec execute = t => {
    switch (t->hasExecuted, t->shouldEnd) {
    | (true, _) => {...t, state: #INF}
    | (_, true) => {...t, state: #DONE}
    | (_, _) => t->executeInstruction->execute
    }
  }
}

let parseLine = l => {
  l
  ->Js.String2.trim
  ->Js.String2.split(_, " ")
  ->(
    x => {
      let op = x[0]->Option.getExn
      let arg = x[1]->Option.getExn
      Instruction.make(op, arg)
    }
  )
}

let parse = data => data->splitNewline->Array.map(parseLine)

let solvePart1 = data => {
  let m = data->parse->Machine.make
  //  let res = "234"->Int.fromString
  //  res
  m->Machine.execute->Machine.accumulator
}

let genPatched = p => {
  p->Array.reduceWithIndex([], (a, x, i) => {
    switch x->Instruction.opcode {
    | #NOP =>
      // return a copy of original program with patch
      let patched = p->Array.copy
      patched->Array.set(i, x->Instruction.setOpcode(#JMP))->ignore
      a->Array.concat([patched])
    | #JMP => {
        // return a copy of original program with patch
        let patched = p->Array.copy
        patched->Array.set(i, x->Instruction.setOpcode(#NOP))->ignore
        a->Array.concat([patched])
      }
    | _ => a
    }
  })
}

let solvePart2 = data => {
  let p = data->parse
  let patched = p->genPatched
  let rans = patched->Array.map(x => {x->Machine.make(_)->Machine.execute})
  let res = rans->Array.keep(x => {
    switch x->Machine.state {
    | #DONE => true
    | _ => false
    }
  })
  res[0]->Option.getExn->Machine.accumulator
}
