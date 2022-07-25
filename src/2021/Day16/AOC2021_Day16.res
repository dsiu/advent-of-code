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
  type payload =
    | Payload_Literal(int)
    | Payload_Operator(string)
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

  let operatorTypeId = typeId

  let literalTypeId: typeId_ = {
    // 100 == 4
    P.char('1')
    ->P.andThen(P.char('0'))
    ->P.andThen(P.char('0'))
    ->P.map((((a, b), c)) => {
      TypeID([a, b, c]->binCharArrayToInt)
    })
  }

  type payload_ = P.t<payload>
  let literalPayload: payload_ = {
    let oneAndFourBit = P.char('1')->P.andThen(fourBinDigitsToStr)
    let zeroAndFourBit = P.char('0')->P.andThen(fourBinDigitsToStr)

    let literal = P.many(oneAndFourBit)->P.andThen(zeroAndFourBit)

    literal->P.map(((xs, last_x)) => {
      Payload_Literal((xs->List.reduce("", (a, (c, x)) => a ++ x) ++ last_x->snd)->binToInt)
    })
  }

  let operatorPayload: payload_ = {
    P.many(binDigit)->P.map(chars => stringifyCharList(chars))->P.map(x => Payload_Operator(x))
  }

  type packet_ = P.t<packet>
  let packet: packet_ = {
    let literalAndPayload = literalTypeId->P.andThen(literalPayload)
    let operatorAndPayload = operatorTypeId->P.andThen(operatorPayload)

    version
    ->P.andThen(P.choice([literalAndPayload, operatorAndPayload]))
    ->P.map(((v, (t, p))) => {
      Packet(v, t, p)
    })
  }

  let dumpPacket = (p: packet) => {
    let Packet(Version(version), TypeID(typeId), p) = p
    switch p {
    | Payload_Literal(l) => j`ver = $version | typeId = $typeId | payload = $l`
    | Payload_Operator(o) => j`ver = $version | typeId = $typeId | payload = $o`
    }
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

  let l = Packet_M.parse("110100101111111000101000")
  l->Result.isOk->log
  l->Result.getExn->fst->dumpPacket->log
  l->Result.getExn->snd->log

  let o = Packet_M.parse("00111000000000000110111101000101001010010001001000000000")
  o->Result.isOk->log
  o->Result.getExn->fst->dumpPacket->log
  o->Result.getExn->snd->log
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
