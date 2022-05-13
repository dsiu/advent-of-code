open Belt
open Utils
let log = Js.Console.log

module Paper = {
  type mark = [#"#" | #"."]
  type t = Array2D.t<mark>

  //    let make = (coords: array<(int, int)>) => {
  //    }
}

let parse = data => {
  let parsed = data->splitDoubleNewline
  let coords = parsed[0]->Option.getExn
  let folds = parsed[1]->Option.getExn

  open Js.String2
  open Option
  (
    coords
    ->splitNewline
    ->Array.map(x => {
      let s = x->trim->split(",")
      (s[0]->getExn->Int.fromString, s[1]->getExn->Int.fromString)
    }),
    folds
    ->splitNewline
    ->Array.map(x => {
      let i = length("fold along ")
      let sub = x->trim->slice(~from=i, ~to_=x->length)
      let s = sub->split("=")
      (s[0]->getExn, s[1]->getExn->Int.fromString)
    }),
  )
}

let solvePart1 = data => {
  let (coords, folds) = data->parse
  coords->Js.log2("coords", _)
  folds->Js.log2("folds", _)
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
