open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

module P = Res_parser
module Rjs = ReScriptJs.Js

module Packet_M = {
  type version = Version(string)
  type typeId = TypeID(string)
  type payload = Payload(string)
  type packet = P(version, typeId, payload)

  let rec concatStringList = chars => {
    switch chars {
    | list{} => ""
    | list{head, ...rest} => head ++ concatStringList(rest)
    }
  }

  let charToString = c => c->int_of_char->Js.String.fromCharCode

  let stringifyCharList = chars => {
    chars->Belt.List.map(charToString)->concatStringList
  }

  let binDigit = P.satisfy(c => c == '0' || c == '1')

  let threeBinDigits = {
    binDigit->P.andThen(binDigit)->P.andThen(binDigit)
  }

  type version_ = P.t<version>
  let version: version_ = threeBinDigits->P.map((((a, b), c)) => {
    Version(a->charToString ++ b->charToString ++ c->charToString)
  })

  type typeId_ = P.t<typeId>
  let typeId: typeId_ = threeBinDigits->P.map((((a, b), c)) => {
    TypeID(a->charToString ++ b->charToString ++ c->charToString)
  })

  type payload_ = P.t<payload>
  let payload = P.many(binDigit)->P.map(chars => stringifyCharList(chars))->P.map(x => Payload(x))

  let parser = version->P.andThen(typeId)->P.andThen(payload)

  type result = P.parseResult<((version, typeId), payload)>
  let parse = (s): result => P.run(parser, s)

  let hexStrToBinStr = s => {
    s
    ->Rjs.Int.fromString(~radix=16, _)
    ->Option.flatMap(x => Rjs.Int.toStringWithRadix(~radix=2, x)->Some)
  }
}

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  open Packet_M
  data->ignore

  let r = Packet_M.parse("110100101111111000101000")
  r->Result.isOk->log
  let (((Version(v), TypeID(t)), Payload(p)), s) = r->Result.getExn
  v->log2("v", _)
  t->log2("t", _)
  p->log2("p", _)
  s->log2("b", _)

  1
}

let solvePart2 = data => {
  data->ignore
  2
}
