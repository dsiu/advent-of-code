open Belt
open Utils
let log = Js.Console.log

module Program = {
  type memory = {
    address: int,
    value: int,
  }

  type memory_space = MutableMap.Int.t<int>

  type t = {
    mask: string,
    mask_passthur: int,
    mask_one: int,
    mask_zero: int,
    instructions: array<memory>,
  }

  //@scope("Math") @val
  @val
  external parseInt: (~x: string, ~base: int) => int = "parseInt"

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

  let make = ((mask, instructions)): t => {
    {
      mask: mask,
      mask_passthur: mask->makePassThurMask,
      mask_one: mask->makeOneMask,
      mask_zero: mask->makeZeroMask,
      instructions: instructions->Array.map(x => {
        {
          address: x[1]->Option.flatMap(Int.fromString)->Option.getExn,
          value: x[2]->Option.flatMap(Int.fromString)->Option.getExn,
        }
      }),
    }
  }
  let base2 = Js.Int.toStringWithRadix(_, ~radix=2)

  let run = t => {
    let mem = MutableMap.Int.make()
    t.instructions->Array.forEach(i => {
      mem->MutableMap.Int.update(i.address, v => {
        v->ignore
        //        "run"->log
        //        t.mask_passthur->base2->log
        let ret = lxor(t.mask_passthur, i.value)->lor(t.mask_one)->land(t.mask_zero->lnot)
        //        ret->log
        //        ret->base2->log
        Some(ret)
      })
    })
    mem
  }

  let dump = t => {
    "dump"->log
    t.mask->log
    t.mask_passthur->base2->log
    t.mask_one->base2->log
    t.mask_zero->base2->log

    t.instructions->log
  }
}

let parse = data => {
  let lines =
    data
    ->splitNewline
    ->Array.map(x => {
      x->Js.String2.trim
    })

  let mask_line = lines[0]->Option.getExn->Js.String2.sliceToEnd(~from="mask = "->String.length)
  let program_lines = lines->Array.sliceToEnd(1)

  let prog_line_re = %re("/mem\[(\d+)\]\s*=\s*(\d+)/i")
  let instructions = program_lines->Array.map(x => {
    prog_line_re
    ->Js.Re.exec_(_, x)
    ->Option.getExn
    ->Js.Re.captures
    ->Array.map(l => l->Js.Nullable.toOption->Option.getExn)
    //->Js.Nullable.toOption->Option.getExn)
  })

  (mask_line, instructions)
}

let solvePart1 = data => {
  let prog = data->parse->Program.make
  prog->Program.dump
  prog->Program.run->Utils.dump_mutableMapInt_of_int_base2
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
