open Belt
open Utils
let log = Js.Console.log
let log2 = Js.Console.log2
let log3 = Js.Console.log3

module P = Res_parser
module Rjs = ReScriptJs.Js

@val
external hexToInt: (string, @as(16) _) => int = "parseInt"

@val
external binToInt: (string, @as(2) _) => int = "parseInt"

let hexStrToBinStr = s => {
  s
  ->splitChars
  ->Array.reduce("", (acc, c) => {
    //    log3(c, c->hexToInt, c->hexToInt->Rjs.Int.toStringWithRadix(~radix=2, _))
    acc ++ c->hexToInt->Rjs.Int.toStringWithRadix(~radix=2, _)
  })
}

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

let binCharListToStr = xs => xs->List.reduce("", (a, v) => a ++ v->charToString)
let binCharListToInt = xs => xs->binCharListToStr->binToInt

module Packet_M = {
  // Types
  //
  type version = Version(int)
  type typeId = TypeID(int)

  type operator_type_0

  type rec packet = Packet(version, typeId, payload)
  and payload =
    | Payload_Literal(int, string)
    | Payload_Operator(operator)
  and operator =
    | Operator_Type_0(int, int, list<packet>) // 15 bits indicate length of bits for sub-packets
    | Operator_Type_1(int, int, list<packet>) // 11 bits indicates number of sub-packets

  // bit utils
  let binDigit = P.satisfy(c => c == '0' || c == '1')

  // make a sequence of the same parser N times
  let sequenceN = (parser, n) => {
    List.makeBy(n, _ => parser)->P.sequence
  }

  let binDigits_3 = sequenceN(binDigit, 3)
  let binDigits_3_int = binDigits_3->P.map(binCharListToInt)
  let binDigits_3_str = binDigits_3->P.map(binCharListToStr)

  let binDigits_4 = sequenceN(binDigit, 4)
  let binDigits_4_int = binDigits_4->P.map(binCharListToInt)
  let binDigits_4_str = binDigits_4->P.map(binCharListToStr)

  let binDigits_15 = sequenceN(binDigit, 15)
  let binDigits_15_int = binDigits_15->P.map(binCharListToInt)
  let binDigits_15_str = binDigits_15->P.map(binCharListToStr)

  let binDigits_11 = sequenceN(binDigit, 11)
  let binDigits_11_int = binDigits_11->P.map(binCharListToInt)
  let binDigits_11_str = binDigits_11->P.map(binCharListToStr)

  type version_ = P.t<version>
  let version: version_ = binDigits_3_int->P.map(x => {
    Version(x)
  })

  type typeId_ = P.t<typeId>
  let typeId: typeId_ = binDigits_3_int->P.map(x => {
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

  //  let takeBitsToStr = n => {
  //    takeN(n, binDigit)->P.map(binCharListToStr)
  //  }

  type payload_ = P.t<payload>

  type restOfMultipleOf4Bits<'a, 'b> = P.t<'a> => P.t<'b>
  let restOfMultipleOf4Bits: restOfMultipleOf4Bits<'a, 'b> = parser => P.Parser(
    input => {
      let result = P.runOnInput(parser, input)

      switch result {
      | Ok((p1Result, inputAfterP1)) =>
        let (xs, last_x) = p1Result
        let len =
          xs->List.reduce(0, (a, (_, x)) => a + x->String.length + 1) +
          last_x->snd->String.length + 1

        let reminder = mod(len, 4)

        log(j`reminder = $reminder | len = $len`)

        let reminderP = sequenceN(binDigit, reminder)->P.map(binCharListToStr)
        let reminderResult = P.runOnInput(reminderP, inputAfterP1)

        switch reminderResult {
        | Ok((resultAfterReminder, inputAfterReminder)) =>
          Ok((p1Result, resultAfterReminder), inputAfterReminder)
        | Error(err) => Error(err)
        }
      | Error(err) => Error(err)
      }
    },
  )

  let literalPayload: payload_ = {
    let oneAndFourBit = P.char('1')->P.andThen(binDigits_4_str)
    let zeroAndFourBit = P.char('0')->P.andThen(binDigits_4_str)

    let literal_payload = P.many(oneAndFourBit)->P.andThen(zeroAndFourBit)->restOfMultipleOf4Bits // ->P.andThen(restOfMultipleOf4Bits)

    literal_payload->P.map((((xs, last_x), rest)) => {
      Payload_Literal((xs->List.reduce("", (a, (_, x)) => a ++ x) ++ last_x->snd)->binToInt, rest)
    })
  }

  let remainingBinDigitStr = P.many(binDigit)->P.map(stringifyCharList)

  type packet_ = P.t<packet>

  let packet: packet_ = P.makeRecursive(p => {
    let operatorPayload: payload_ = {
      let lengthType_0 =
        P.char('0')
        ->P.andThen(binDigits_15_int)
        ->P.andThen(P.many(p))
        ->P.andThen(remainingBinDigitStr)
      let lengthType_1 =
        P.char('1')
        ->P.andThen(binDigits_11_int)
        ->P.andThen(P.many(p))
        ->P.andThen(remainingBinDigitStr)

      P.choice([lengthType_0, lengthType_1])->P.map(((
        ((len_type, len), rest_packets),
        rest_str,
      )) => {
        switch len_type {
        | '0' => Payload_Operator(Operator_Type_0(0, len, rest_packets))

        | '1' => Payload_Operator(Operator_Type_1(1, len, rest_packets))
        }
      })
    }

    let literalAndPayload = literalTypeId->P.andThen(literalPayload)
    let operatorAndPayload = operatorTypeId->P.andThen(operatorPayload)

    version
    ->P.andThen(P.choice([literalAndPayload, operatorAndPayload]))
    ->P.map(((version, (typeId, payload))) => {
      Packet(version, typeId, payload)
    })
  })

  let rec dumpPacket = (p: packet) => {
    let Packet(Version(version), TypeID(typeId), p) = p
    let rest_packet_str = rest => rest->List.reduce("", (a, p) => a ++ "\n    " ++ dumpPacket(p))

    switch p {
    | Payload_Literal(
        l,
        rest,
      ) => j`ver = $version | typeId = $typeId | literal payload = $l | rest = $rest`
    | Payload_Operator(o) =>
      switch o {
      | Operator_Type_0(len_t, len, rest) => {
          let sub_packets_str = rest_packet_str(rest)

          j`{ ver = $version | typeId = $typeId | op payload = type_0($len_t, n_bits: $len, $sub_packets_str) }\n`
        }
      | Operator_Type_1(len_t, len, rest) => {
          let sub_packets_str = rest_packet_str(rest)

          j`{ ver = $version | typeId = $typeId | op payload = type_1($len_t, n_packats: $len, $sub_packets_str) }\n`
        }
      }
    }
  }

  type result = P.parseResult<packet>
  let parse = (s): result => P.run(packet, s)
}

let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  open Packet_M
  data->ignore

  let data = [
    "110100101111111000101000", // literal 2021
    //    "00111000000000000110111101000101001010010001001000000000", // op, type 0, len 27
    //    "11101110000000001101010000001100100000100011000001100000", // op, type 1, len 3

    //    "11010001010",  // literal 10
    //    "0101001000100100", // literal 20
  ]

  let data_hex = [
    "8A004A801A8002F478",

    //represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.
  ]->Array.map(x => x->hexStrToBinStr)

  data_hex->Array.forEach(d => {
    let l = Packet_M.parse(d)
    l->Result.isOk->log2(d)
    l->Result.getExn->fst->dumpPacket->log
    l->Result.getExn->snd->log
    log("\n")
  })
  1
}

let solvePart2 = data => {
  data->ignore
  2
}
