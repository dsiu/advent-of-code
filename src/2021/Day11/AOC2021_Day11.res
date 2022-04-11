open Belt
open Utils
let log = Js.Console.log

module Octopus = {
  type t = Array2D.t<int>

  let incEnergyLevel = t => {
    t->Array2D.map(add(1))
  }

  //  let flash = t => {
  //
  //  }
}

let parse = data =>
  data
  ->splitNewline
  ->Array.map(
    FP_Utils.compose(Js.String2.trim, x => x->Utils.splitChars->Array.map(intFromStringExn)),
  )

let solvePart1 = data => {
  let d = data->parse
  d->Js.log
  d->Array2D.lengthX->Js.log2("lengthX")
  d->Array2D.lengthY->Js.log2("lengthY")

  let e = d->Octopus.incEnergyLevel
  e->Js.log
  e->Array2D.lengthX->Js.log2("lengthX")
  e->Array2D.lengthY->Js.log2("lengthY")

  1
}

let solvePart2 = data => {
  data->ignore
  2
}
