open Belt
open Utils
module TC = Tablecloth

let log = Js.Console.log

type contents = Set.String.t
type rucksack = Rucksack(array<string>, array<string>)

let parse: 'a. (string, array<string> => 'a) => array<'a> = (data, fn) => {
  data
  ->splitNewline
  ->Array.map(s => {
    let t = s->Js.String2.trim->splitChars
    fn(t)
  })
}

let stringToChar = String.get(_, 0)

let charToPriority: string => int = item => {
  let c = item->stringToChar
  let lowerA = Char.code('a')
  let upperA = Char.code('A')
  c->TC.Char.isUppercase ? c->TC.Char.toCode - upperA + 1 + 26 : c->TC.Char.toCode - lowerA + 1
}

let commonItem: rucksack => string = (Rucksack(a, b)) => {
  module S = Set.String
  S.intersect(S.fromArray(a), S.fromArray(b))->S.toArray->Array.getExn(0)
}

let part1: array<rucksack> => int = rucksacks => {
  rucksacks->Array.map(compose(commonItem, charToPriority))->sumIntArray
}

//let badgeOf: array<rucksack> => string = rucksacks => {
//  S.interact()
//}

let part2: array<rucksack> => int = rucksacks => {
  let groups = rucksacks->TC.Array.chunksOf(~size=3)
  //  let badges = groups->TC.Array.map(~f=badgeOf)
  groups->log

  2
  //  badges->Array.map(charToPriority)->sumIntArray
}

let mkRucksack: array<string> => rucksack = xs => {
  open Array
  let mid = xs->length / 2
  Rucksack(xs->slice(~offset=0, ~len=mid), xs->slice(~offset=mid, ~len=mid))
}

let solvePart1 = data => {
  data->parse(mkRucksack)->part1
}

let solvePart2 = data => {
  data->parse(identity)->part2
  2
}
