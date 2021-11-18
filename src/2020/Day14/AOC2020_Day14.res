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
      mask_passthur: int,
      mask_one: int,
      mask_zero: int,
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
      // xx1xx0xx -> 110110xx
      m->Utils.splitChars->Array.map(f)->Js.Array2.joinWith("")->parseInt(~x=_, ~base=2)
    }

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
      //      t.mask_passthur->base2->log
      //      t.mask_one->base2->log
      //      t.mask_zero->base2->log
    }
  }

  module Memory = {
    type t = {
      address: int,
      value: int,
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
        address: parsed[1]->Option.flatMap(Int.fromString)->Option.getExn,
        value: parsed[2]->Option.flatMap(Int.fromString)->Option.getExn,
      }
    }

    let dump = t => {
      "=== Memory dump ==="->log
      t->log
    }
  }

  type memory_space = MutableMap.Int.t<int>

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
      memory: MutableMap.Int.make(),
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
        t.memory->MutableMap.Int.update(i.address, v => {
          i->Memory.dump
          v->ignore
          //          let m = cur_m.contents
          let ret =
            land(cur_m.contents.mask_passthur, i.value)
            ->lor(cur_m.contents.mask_one)
            ->land(cur_m.contents.mask_zero->lnot)

          Some(ret->Utils.int32ToUint32)
        })
      }
    })

    t.memory
  }

  //  instructions: instructions->Array.map(x => {
  //          {
  //            address: x[1]->Option.flatMap(Int.fromString)->Option.getExn,
  //            value: x[2]->Option.flatMap(Int.fromString)->Option.getExn,
  //          }
  //        }),

  //  let run = t => {
  //    let mem = MutableMap.Int.make()
  //    t.instructions->Array.forEach(i => {
  //      mem->MutableMap.Int.update(i.address, v => {
  //        v->ignore
  //        let ret = land(t.mask_passthur, i.value)->lor(t.mask_one)->land(t.mask_zero->lnot)
  //        Some(ret)
  //      })
  //    })
  //    mem
  //  }

  //  let run = t => {
  //    _
  //  }

  let dump = t => {
    "=== Program dump ==="->log
    t.instructions->log
    t.memory->Utils.dump_mutableMapInt_of_int
  }
}

let parse = data => {
  data
  ->splitNewline
  ->Array.map(x => {
    x->Js.String2.trim
  })
}

//let parse = data => {
//  let lines =
//    data
//    ->splitNewline
//    ->Array.map(x => {
//      x->Js.String2.trim
//    })
//
//  let mask_line = lines[0]->Option.getExn->Js.String2.sliceToEnd(~from="mask = "->String.length)
//  let program_lines = lines->Array.sliceToEnd(1)
//
//  let prog_line_re = %re("/mem\[(\d+)\]\s*=\s*(\d+)/i")
//  let instructions = program_lines->Array.map(x => {
//    prog_line_re
//    ->Js.Re.exec_(_, x)
//    ->Option.getExn
//    ->Js.Re.captures
//    ->Array.map(l => l->Js.Nullable.toOption->Option.getExn)
//    //->Js.Nullable.toOption->Option.getExn)
//  })
//
//  (mask_line, instructions)
//}

let solvePart1 = data => {
  let prog = data->parse->Program.make
  prog->Program.dump
  let result = prog->Program.run
  "=== part 1 result dump ==="->log
  //  result->Utils.dump_mutableMapInt_of_int_base2
  result->Utils.dump_mutableMapInt_of_int
  open ReScriptJs.Js
  let answer = result->MutableMap.Int.reduce(BigInt.fromInt(0), (a, k, v) => {
    k->ignore
    v->BigInt.fromInt->BigInt.add(a)
  })
  answer->BigInt.toString
}

let solvePart2 = data => {
  data->ignore
  2
}
