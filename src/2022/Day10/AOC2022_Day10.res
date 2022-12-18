open Stdlib
open Utils
let log = Js.Console.log

type operation =
  | Noop
  | Addx(int)

@@warning("-8")
let parse = data => {
  data
  ->splitNewline
  ->Array.map(x => {
    let l = x->Js.String2.trim
    l->String.startsWith("noop")
      ? Noop
      : {
          let [_a, v] = l->String.split(" ")
          Addx(v->int_of_string)
        }
  })
}

let solvePart1 = data => {
  data->parse->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
