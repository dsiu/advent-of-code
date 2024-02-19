open Belt
open Utils

let toInt64 = Utils.int64FromBitString

module Program = {
  module Mask = {
    type t = {
      mask: string,
      mask_x: Int64.t,
      mask_x_str: string, // need to keep the string for easy parsing of bits
      mask_one: Int64.t,
      mask_zero: Int64.t,
    }

    let onlyXto1 = c => {
      switch c {
      | "X" => "1"
      | _ => "0"
      }
    }

    let only1to1 = c => {
      switch c {
      | "1" => "1"
      | _ => "0"
      }
    }

    let only0to1 = c => {
      switch c {
      | "0" => "1"
      | _ => "0"
      }
    }

    let makeMask = (m, f) => {
      m->Utils.splitChars->Array.map(f)->Js.Array2.joinWith("")
    }

    let toString = ({mask, mask_x, mask_x_str, mask_one, mask_zero}: t) => {
      `mask: ${mask}\n` ++
      `mask_x: ${mask_x->Int64.to_string}\n` ++
      `mask_x_str: ${mask_x_str}` ++
      `mask_one: ${mask_one->Int64.to_string}\n` ++
      `mask_zero: ${mask_zero->Int64.to_string}`
    }

    let makePassThurMask = makeMask(_, onlyXto1)
    let makeOneMask = makeMask(_, only1to1)
    let makeZeroMask = makeMask(_, only0to1)

    let make = str => {
      // str = "mask = 11100XX0000X1101X1010100X1010001XX0X"
      {
        mask: str->Js.String2.sliceToEnd(~from="mask = "->String.length),
        mask_x: str->makePassThurMask->toInt64,
        mask_x_str: str->makePassThurMask,
        mask_one: str->makeOneMask->toInt64,
        mask_zero: str->makeZeroMask->toInt64,
      }
    }
  }

  module Memory = {
    type t = {
      address: Int64.t,
      value: Int64.t,
    }

    let make = str => {
      // str = "mem[22535] = 42768"
      let prog_line_re = %re("/mem\[(\d+)\]\s*=\s*(\d+)/i")
      let parsed =
        prog_line_re
        ->Js.Re.exec_(_, str)
        ->Option.getExn
        ->Js.Re.captures
        ->Array.map(l => l->Js.Nullable.toOption->Option.getExn)

      {
        address: parsed[1]->Option.map(Int64.of_string)->Option.getExn,
        value: parsed[2]->Option.map(Int64.of_string)->Option.getExn,
      }
    }

    let toString = t => {
      `address: ${t.address->Int64.to_string}\n` ++ `value: ${t.value->Int64.to_string}`
    }
  }

  // map key is the address in Int64.  It should be MutableMap.Int64.t but it isn't implemented in ReScript.
  // So, let's use Int64 as string as map key here
  type memory_space = MutableMap.String.t<Int64.t>

  type instruction = MaskOp(Mask.t) | MemOp(Memory.t)

  let instructionToString = i => {
    switch i {
    | MaskOp(mask) => "MaskOp(" ++ Mask.toString(mask) ++ ")"
    | MemOp(mem) => "MemOp(" ++ Memory.toString(mem) ++ ")"
    }
  }

  type t = {
    instructions: array<instruction>,
    memory: memory_space,
  }

  exception InvalidInstruction(string)

  let parse = lines => {
    lines->Array.map(l => {
      switch l->Js.String2.substring(~from=0, ~to_=4) {
      | "mem[" => l->Memory.make->MemOp
      | "mask" => l->Mask.make->MaskOp
      | _ => raise(InvalidInstruction(l))
      }
    })
  }

  let make = instructions => {
    {
      instructions: instructions->parse,
      memory: MutableMap.String.make(),
    }
  }

  // part 1: change memory value based on mask
  let decodeMemory = (mask: Mask.t, mem_value) => {
    Int64.logand(mask.mask_x, mem_value)
    ->Int64.logor(mask.mask_one)
    ->Int64.logand(mask.mask_zero->Int64.lognot)
  }

  let part1Algo = (space: memory_space, mask: Mask.t, mem: Memory.t) => {
    space->MutableMap.String.update(mem.address->Int64.to_string, v => {
      v->ignore
      decodeMemory(mask, mem.value)->Some
    })
  }

  // return indexes (lsb=0) of 1 as if string is a binary string
  let bit1Index = m => {
    let xs = m->Utils.splitChars
    let len = xs->Array.length

    xs->Belt.Array.reduceWithIndex([], (a, x, i) => {
      x == "1" ? Array.concat(a, [len - 1 - i]) : a
    })
  }

  let decodeAddress = (mask: Mask.t, mem_address) => {
    let pos_mask = mask.mask_x->Int64.lognot
    let base = Int64.logand(Int64.logor(mem_address, mask.mask_one), pos_mask)

    let pos = mask.mask_x_str->bit1Index
    let all_pos = pos->Powerset.powersetArray

    let decoded_addresses = all_pos->Array.map(pos => {
      let m = pos->Array.reduce(Int64.zero, (acc, x) => {
        Int64.logor(acc, Int64.shift_left(Int64.one, x))
      })
      Int64.logor(base, m)->Int64.to_string
    })
    decoded_addresses
  }

  let part2Algo = (space: memory_space, mask: Mask.t, mem: Memory.t) => {
    let addresses = decodeAddress(mask, mem.address)
    addresses->Array.forEach(addr => {
      space->MutableMap.String.update(addr, v => {
        v->ignore
        mem.value->Some
      })
    })
  }

  let run = (t, algo) => {
    let cur_m = ref(Mask.make("mask = 11110000XXXX"))
    t.instructions->Array.forEach(x => {
      switch x {
      | MaskOp(mask) => cur_m := mask
      | MemOp(mem) => algo(t.memory, cur_m.contents, mem)
      }
    })

    t.memory
  }

  let runPart1 = run(_, part1Algo)
  let runPart2 = run(_, part2Algo)

  let toString = t => {
    "=== Program dump ===\n" ++
    t.instructions->Printable.Array.toString(instructionToString) ++
    "\n" ++
    t.memory->Printable.MutableMapString.Int64.toString
  }
}

let parse = data => {
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim
  })
}

let memoryToAnswer = MutableMap.String.reduce(_, Int64.of_int(0), (a, k, v) => {
  k->ignore
  Int64.add(v, a)
})

let solvePart1 = data => {
  let prog = data->parse->Program.make
  //  prog->Program.dump
  let result = prog->Program.runPart1
  //  "=== part 1 result dump ==="->log
  //  result->Utils.dump_mutableMapInt_of_int64
  let answer = result->memoryToAnswer
  answer
}

let solvePart2 = data => {
  let prog = data->parse->Program.make
  //  prog->Program.dump
  let result = prog->Program.runPart2
  //  "=== part 2 result dump ==="->log
  //  result->Utils.dump_mutableMapInt_of_int64
  let answer = result->memoryToAnswer
  answer
}
