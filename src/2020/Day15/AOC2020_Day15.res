open Belt
open Utils
let log = Js.Console.log
module Log = unpack(JS_Debug.make(__MODULE__))

module Appear = {
  type t = array<int>

  let get_last_2_diff = t => {
    let len = t->Array.length
    switch len {
    | 0
    | 1 =>
      Some(0)
    | _ => Some(t->Array.getUnsafe(len - 1) - t->Array.getUnsafe(len - 2))
    }
  }

  let append = (t, x) => {
    let len = t->Array.length
    switch len {
    | 0 => [x]
    | 1 => [t->Array.getUnsafe(0), x]
    | _ => [t->Array.getUnsafe(len - 1), x]
    }
  }
}

let solve = (xs, total_turn) => {
  let spoken = xs->Array.reduceWithIndexU(Map.Int.empty, (. a, x, i) => a->Map.Int.set(x, [i + 1]))

  let len = xs->Array.length
  let rec inner = (spoken, last_num, this_turn: int, total_turn) => {
    Log.debug(this_turn->Int.toString)

    //    Js.log2("last_num", last_num)
    this_turn > total_turn
      ? last_num
      : {
          let last_num_appear = spoken->Map.Int.get(last_num)

          let next_num = last_num_appear->Option.flatMap(Appear.get_last_2_diff)->Option.getExn

          //          Js.log2("last_appear", last_num_appear)
          //          Js.log2("next", next_num)

          let next_spoken = spoken->Map.Int.update(next_num, next_num_appear => {
            switch next_num_appear {
            | Some(a) => a->Appear.append(this_turn)->Some
            | None => [this_turn]->Some
            }
          })

          inner(next_spoken, next_num, this_turn + 1, total_turn)
        }
  }
  inner(spoken, xs->Array.getExn(len - 1), len + 1, total_turn)
}

let parse = data =>
  data->Js.String2.trim->Js.String2.split(",")->Array.map(x => x->Int.fromString->Option.getExn)

let solvePart1 = data => {
  data->parse->solve(2020)
}

let solvePart2 = data => {
  data->parse->solve(30000000)
  //  data->parse->solve(300000)
}
