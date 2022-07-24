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
  type packet = Packet(version, typeId, payload)

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

  type packet_ = P.t<packet>
  let packet: packet_ =
    version
    ->P.andThen(typeId)
    ->P.andThen(payload)
    ->P.map((((a, b), c)) => {
      Packet(a, b, c)
    })

  let dumpPacket = (Packet(Version(version), TypeID(typeId), Payload(payload))) => {
    `ver = ${version} | typeId = ${typeId} | payload = ${payload}`
  }

  type result = P.parseResult<packet>
  let parse = (s): result => P.run(packet, s)

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

  r->Result.getExn->fst->dumpPacket->log
  r->Result.getExn->snd->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
