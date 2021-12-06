open Belt
open Utils
let log = Js.Console.log

//@scope("Math") @val
@val
external parseInt: (~x: string, ~base: int) => int = "parseInt"

let base2 = Js.Int.toStringWithRadix(_, ~radix=2)

module Program = {
  module Mask = {
    type t = {
      mask: string,
      mask_passthur: string,
      mask_one: string,
      mask_zero: string,
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
      //      ("0b" ++ m->Utils.splitChars->Array.map(f)->Js.Array2.joinWith(""))->Int64.of_string
      m->Utils.splitChars->Array.map(f)->Js.Array2.joinWith("")
    }

    let int64FromBitString = str => ("0b" ++ str)->Int64.of_string

    let makePassThurMask = makeMask(_, onlyXto1)
    let makeOneMask = makeMask(_, only1to1)
    let makeZeroMask = makeMask(_, only0to1)

    let make = str => {
      // str = "mask = 11100XX0000X1101X1010100X1010001XX0X"
      {
        mask: str->Js.String2.sliceToEnd(~from="mask = "->String.length),
        mask_passthur: str->makePassThurMask,
        mask_one: str->makeOneMask,
        mask_zero: str->makeZeroMask,
      }
    }

    let dump = t => {
      "=== Mask dump ==="->log
      t.mask->log
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

    let dump = t => {
      "=== Memory dump ==="->log
      t->log
    }
  }

  // map key is the address in Int64.  It should be MutableMap.Int64.t but it isn't implemented in ReScript.
  // So, let's use Int64 as string as map key here
  type memory_space = MutableMap.String.t<Int64.t>

  type instruction = Mask(Mask.t) | Mem(Memory.t)

  type t = {
    instructions: array<instruction>,
    memory: memory_space,
  }

  exception InvalidInstruction(string)

  let parseInstructions = lines => {
    lines->Array.map(l => {
      switch l->Js.String2.substring(~from=0, ~to_=4) {
      | "mem[" => l->Memory.make->Mem
      | "mask" => l->Mask.make->Mask
      | _ => raise(InvalidInstruction(l))
      }
    })
  }

  let make = instructions => {
    {
      instructions: instructions->parseInstructions,
      memory: MutableMap.String.make(),
    }
  }

  let run = t => {
    let cur_m = ref(Mask.make("mask = 11110000XXXX"))
    t.instructions->Array.forEach(x => {
      switch x {
      | Mask(m) => {
          cur_m := m
          cur_m.contents->Mask.dump
        }
      | Mem(i) =>
        t.memory->MutableMap.String.update(i.address->Int64.to_string, v => {
          i->Memory.dump
          v->ignore
          let ret =
            Int64.logand(cur_m.contents.mask_passthur->Mask.int64FromBitString, i.value)
            ->Int64.logor(cur_m.contents.mask_one->Mask.int64FromBitString)
            ->Int64.logand(cur_m.contents.mask_zero->Mask.int64FromBitString->Int64.lognot)
          Some(ret)
        })
      }
    })

    t.memory
  }

  let dump = t => {
    "=== Program dump ==="->log
    t.instructions->log
    t.memory->Utils.dump_mutableMapString_of_int64
  }
}

let parse = data => {
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim
  })
}

let solvePart1 = data => {
  let prog = data->parse->Program.make
  prog->Program.dump
  let result = prog->Program.run
  //  "=== part 1 result dump ==="->log
  //  result->Utils.dump_mutableMapInt_of_int64
  //  open ReScriptJs.Js
  let answer = result->MutableMap.String.reduce(Int64.of_int(0), (a, k, v) => {
    k->ignore
    Int64.add(v, a)
  })
  answer->Int64.to_string->log
  answer
}

let solvePart2 = data => {
  data->ignore
  2
}
