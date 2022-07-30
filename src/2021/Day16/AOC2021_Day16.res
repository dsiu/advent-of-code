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

let hexTable = Js.Dict.fromList(list{
  ("0", "0000"),
  ("1", "0001"),
  ("2", "0010"),
  ("3", "0011"),
  ("4", "0100"),
  ("5", "0101"),
  ("6", "0110"),
  ("7", "0111"),
  ("8", "1000"),
  ("9", "1001"),
  ("A", "1010"),
  ("B", "1011"),
  ("C", "1100"),
  ("D", "1101"),
  ("E", "1110"),
  ("F", "1111"),
})

let hexStrToBinStr = s => {
  s
  ->splitChars
  ->Array.reduce("", (acc, c) => {
    acc ++ hexTable->Js.Dict.unsafeGet(c)
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

exception ParseError(string)

module Packet = {
  // Types
  //
  type version = Version(int)
  type typeId = TypeID(int)

  type operator_type_0

  type rec packet = Packet(version, typeId, payload)
  and payload =
    | Literal(int)
    | Op_Type_0(int, list<packet>) // 15 bits indicate length of bits for sub-packets
    | Op_Type_1(int, list<packet>) // 11 bits indicates number of sub-packets

  // bit utils
  let binDigit = P.satisfy(c => c == '0' || c == '1')

  // make a sequence of the same parser N times
  let sequenceN = (parser, n) => {
    List.makeBy(n, _ => parser)->P.sequence
  }

  // non-recursive version of sequenceN
  let sequenceN_ = (parser, n) => P.Parser(
    input => {
      let rec inner = (parser, i, input, result) => {
        i <= 0
          ? Ok((result->List.reverse, input))
          : switch P.runOnInput(parser, input) {
            | Error(msg) => Error(msg)
            | Ok((result', remaining')) =>
              inner(parser, i - 1, remaining', list{result', ...result})
            }
      }

      inner(parser, n, input, list{})
    },
  )

  let binDigits_3 = sequenceN_(binDigit, 3)
  let binDigits_3_int = binDigits_3->P.map(binCharListToInt)
  let binDigits_3_str = binDigits_3->P.map(binCharListToStr)

  let binDigits_4 = sequenceN_(binDigit, 4)
  let binDigits_4_int = binDigits_4->P.map(binCharListToInt)
  let binDigits_4_str = binDigits_4->P.map(binCharListToStr)

  let binDigits_15 = sequenceN_(binDigit, 15)
  let binDigits_15_int = binDigits_15->P.map(binCharListToInt)
  let binDigits_15_str = binDigits_15->P.map(binCharListToStr)

  let binDigits_11 = sequenceN_(binDigit, 11)
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

  type payload_ = P.t<payload>

  // Literal
  //
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

        log(j`  reminder = $reminder | len = $len`)

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

    let literal_payload = P.many(oneAndFourBit)->P.andThen(zeroAndFourBit) // ->restOfMultipleOf4Bits // ->P.andThen(restOfMultipleOf4Bits)

    literal_payload->P.map(((xs, last_x)) => {
      "literalPayload"->log
      Literal({
        let l = (xs->List.reduce("", (a, (_, x)) => a ++ x) ++ last_x->snd)->binToInt

        j`  l = $l`->log
        l
      })
    })
  }

  let remainingBinDigitStr = P.many(binDigit)->P.map(stringifyCharList)

  type opPayloadType1<'a, 'b> = P.t<'a> => P.t<'b>
  type opPayloadType0<'a, 'b> = P.t<'a> => P.t<'b>

  // Packet
  //
  type packet_ = P.t<packet>

  let packet: packet_ = P.makeRecursive(p => {
    // opPayload Type 0  len = 15 bit (total bit length for packets)
    //
    let opPayloadType0: opPayloadType0<'a, 'b> = parser => P.Parser(
      input => {
        let result = P.runOnInput(parser, input)
        "opPayloadType0"->log

        switch result {
        | Ok((p1Result, inputAfterP1)) => {
            let (c, nBits) = p1Result
            `  c = ${c->charToString}`->log
            j`  nBits = $nBits`->log
            //            let reminderPackets = sequenceN(binDigit, nBits)->P.map(binCharListToStr)
            let reminderPackets = sequenceN_(binDigit, nBits)->P.map(binCharListToStr)
            let reminderResult = P.runOnInput(reminderPackets, inputAfterP1)
            j`  done running reminderpackages`->log
            switch reminderResult {
            | Ok((packetStr, inputAfterReminder)) => {
                j`  packetStr = $packetStr`->log

                let packetsResult = P.run(P.many(p), packetStr)

                switch packetsResult {
                | Ok((resultAfterReminder, _)) =>
                  Ok((p1Result, resultAfterReminder), inputAfterReminder)
                | Error(err) => {
                    "  Error many(p)"->log
                    Error(err)
                  }
                }
              }
            | Error(err) => {
                "  Error reminderPackets"->log
                Error(err)
              }
            }
          }
        | Error(err) => Error(err)
        }
      },
    )

    // opPayload Type 1  len = 11 bit (number of packets)
    //
    let opPayloadType1: opPayloadType1<'a, 'b> = parser => P.Parser(
      input => {
        let result = P.runOnInput(parser, input)
        "opPayloadType1"->log
        switch result {
        | Ok((p1Result, inputAfterP1)) => {
            let (c, nPacket) = p1Result
            `  c = ${c->charToString}`->log

            j`  nPacket = $nPacket`->log

            let reminderPackets = sequenceN(p, nPacket)
            let reminderPacketsResult = P.runOnInput(reminderPackets, inputAfterP1)

            switch reminderPacketsResult {
            | Ok((resultAfterReminder, inputAfterReminder)) =>
              Ok((p1Result, resultAfterReminder), inputAfterReminder)
            | Error(err) => Error(err)
            }
          }
        | Error(err) => Error(err)
        }
      },
    )

    let operatorPayload: payload_ = {
      let lengthType_0 = P.char('0')->P.andThen(binDigits_15_int)->opPayloadType0
      let lengthType_1 = P.char('1')->P.andThen(binDigits_11_int)->opPayloadType1

      P.choice([lengthType_0, lengthType_1])->P.map((((len_type, len), rest_packets)) => {
        switch len_type {
        | '0' => Op_Type_0(len, rest_packets)
        | '1' => Op_Type_1(len, rest_packets)
        | _ => raise(ParseError("unknown operator len type = " ++ len_type->charToString))
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
    | Literal(l) => j`ver = $version | typeId = $typeId | literal payload = $l`

    | Op_Type_0(len, rest) => {
        let sub_packets_str = rest_packet_str(rest)

        j`{ ver = $version | typeId = $typeId | op payload = type_0(n_bits: $len, $sub_packets_str) }\n`
      }
    | Op_Type_1(len, rest) => {
        let sub_packets_str = rest_packet_str(rest)

        j`{ ver = $version | typeId = $typeId | op payload = type_1(n_packats: $len, $sub_packets_str) }\n`
      }
    }
  }

  let version_sum = (p: packet) => {
    let rec inner = (p, sum) => {
      let Packet(Version(version), TypeID(_), payload) = p
      switch payload {
      | Literal(_) => sum + version
      | Op_Type_0(_, rest)
      | Op_Type_1(_, rest) =>
        version + List.reduce(rest, 0, (a, p) => {a + inner(p, 0)})
      }
    }

    inner(p, 0)
  }

  type result = P.parseResult<packet>
  let parse = (s): result => P.run(packet, s)
}

//let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  open Packet
  let d = data->hexStrToBinStr

  let l = Packet.parse(d)
  l->Result.isOk->log2(d)
  let p = l->Result.getExn->fst
  switch l {
  | Ok(_) => {
      p->dumpPacket->log
      l->Result.getExn->snd->log
      log("\n")
    }
  | Error(err) => {
      log(err)
      log("\n")
    }
  }

  p->Packet.version_sum
}

let solvePart2 = data => {
  data->ignore
  2
}
