open Belt
open Utils
module TC = Tablecloth

let log = Js.Console.log

let parse = data => {
  data
  ->splitNewline
  ->Array.map(s => {
    let t = s->Js.String2.trim->splitChars
    let mid = t->Array.length / 2
    (t->Array.slice(~offset=0, ~len=mid), t->Array.slice(~offset=mid, ~len=mid))
  })
}

let charToPriority = str => {
  let c = str->String.get(0)
  let lowerA = Char.code('a')
  let upperA = Char.code('A')
  c->TC.Char.isUppercase ? c->TC.Char.toCode - upperA + 1 + 26 : c->TC.Char.toCode - lowerA + 1
}

let part1 = xs => {
  xs
  ->Array.map(((a, b)) => {
    module S = Set.String
    S.intersect(S.fromArray(a), S.fromArray(b))->S.toArray->Array.getExn(0)
  })
  ->Array.map(charToPriority)
  ->sumIntArray
}

let solvePart1 = data => {
  data->parse->part1->log
  1->pred->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
