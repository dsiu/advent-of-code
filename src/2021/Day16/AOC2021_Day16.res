open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2

module P = Res_parser
module Rjs = ReScriptJs.Js

@val
external hexToInt: (string, @as(16) _) => int = "parseInt"

@val
external binToInt: (string, @as(2) _) => int = "parseInt"

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

let binCharArrayToStr = xs => xs->Array.reduce("", (a, v) => a ++ v->charToString)
let binCharArrayToInt = xs => xs->binCharArrayToStr->binToInt

module Packet_M = {
  // Types
  //
  type version = Version(int)
  type typeId = TypeID(int)
  type payload = Payload_Literal(int)
  type packet = Packet(version, typeId, payload)

  // bit utils
  let binDigit = P.satisfy(c => c == '0' || c == '1')

  let threeBinDigits = {
    binDigit->P.andThen(binDigit)->P.andThen(binDigit)
  }

  let threeBinDigitsMap = f => {
    threeBinDigits->P.map((((a, b), c)) => {
      [a, b, c]->f
    })
  }

  let threeBinDigitsToInt = threeBinDigitsMap(binCharArrayToInt)
  let threeBinDigitsToString = threeBinDigitsMap(binCharArrayToStr)

  let fourBinDigits = {
    binDigit->P.andThen(binDigit)->P.andThen(binDigit)->P.andThen(binDigit)
  }

  let fourBinDigitsMap = f => {
    fourBinDigits->P.map(((((a, b), c), d)) => {
      [a, b, c, d]->f
    })
  }

  let fourBinDigitsToInt = fourBinDigitsMap(binCharArrayToInt)
  let fourBinDigitsToStr = fourBinDigitsMap(binCharArrayToStr)

  type version_ = P.t<version>
  let version: version_ = threeBinDigitsToInt->P.map(x => {
    Version(x)
  })

  type typeId_ = P.t<typeId>
  let typeId: typeId_ = threeBinDigitsToInt->P.map(x => {
    TypeID(x)
  })

  type payload_ = P.t<payload>
  let payload = {
    let oneAndFourBit = P.char('1')->P.andThen(fourBinDigitsToStr)
    let zeroAndFourBit = P.char('0')->P.andThen(fourBinDigitsToStr)

    let literal = P.many(oneAndFourBit)->P.andThen(zeroAndFourBit)

    literal->P.map(((xs, last_x)) => {
      Payload_Literal((xs->List.reduce("", (a, (c, x)) => a ++ x) ++ last_x->snd)->binToInt)
    })
  }

  type packet_ = P.t<packet>
  let packet: packet_ =
    version
    ->P.andThen(typeId)
    ->P.andThen(payload)
    ->P.map((((a, b), c)) => {
      Packet(a, b, c)
    })

  let dumpPacket = (Packet(Version(version), TypeID(typeId), Payload_Literal(payload))) => {
    j`ver = $version | typeId = $typeId | payload = $payload`
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
