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

module BigInt = ReScriptJs.Js.BigInt
let binStrToInt64 = s => ("0b" ++ s)->BigInt.fromString->BigInt.toFloat->Int64.of_float

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
  type rec packet = Packet(version, typeId, payload)
  and payload =
    | Literal(Int64.t)
    | Op_Len_Kind_0(int, list<packet>) // 15 bits indicate length of bits for sub-packets
    | Op_Len_Kind_1(int, list<packet>) // 11 bits indicates number of sub-packets

  // parse a binary bit
  //
  let binDigit = P.satisfy(c => c == '0' || c == '1')

  // make a sequence of the same parser N times
  //
  let sequenceN = (parser, n) => {
    List.makeBy(n, _ => parser)->P.sequence
  }

  // non-recursive version of sequenceN
  //
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

  // parse pre-set number of bits
  //
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

  // parse version tag
  //
  type version_ = P.t<version>
  let version: version_ = binDigits_3_int->P.map(x => {
    Version(x)
  })

  // parse typeID tag
  //
  type typeId_ = P.t<typeId>
  let typeId: typeId_ = binDigits_3_int->P.map(x => {
    TypeID(x)
  })

  // operator packet has type ID other than literal packet
  //
  let operatorTypeId = typeId

  // literal packet has type ID of 4 == 100
  //
  let literalTypeId: typeId_ = {
    // 100 == 4
    P.char('1')
    ->P.andThen(P.char('0'))
    ->P.andThen(P.char('0'))
    ->P.map((((a, b), c)) => {
      TypeID([a, b, c]->binCharArrayToInt)
    })
  }

  let remainingBinDigitStr = P.many(binDigit)->P.map(stringifyCharList)

  // Packet
  //
  type packet_ = P.t<packet>
  type literalPayload_ = P.t<payload>
  type opPayloadType1_<'a, 'b> = P.t<'a> => P.t<'b>
  type opPayloadType0_<'a, 'b> = P.t<'a> => P.t<'b>

  let packet: packet_ = P.makeRecursive(p => {
    // Literal payload
    //
    let literalPayload: literalPayload_ = {
      let oneAndFourBit = P.char('1')->P.andThen(binDigits_4_str)
      let zeroAndFourBit = P.char('0')->P.andThen(binDigits_4_str)

      let literal_payload = P.many(oneAndFourBit)->P.andThen(zeroAndFourBit) // ->restOfMultipleOf4Bits // ->P.andThen(restOfMultipleOf4Bits)

      literal_payload->P.map(((xs, last_x)) => {
        //      "literalPayload"->log
        Literal({
          (xs->List.reduce("", (a, (_, x)) => a ++ x) ++ last_x->snd)->binStrToInt64
        })
      })
    }

    // opPayload Type 0  len = 15 bit (total bit length for packets)
    //
    let opPayloadType0: opPayloadType0_<'a, 'b> = parser => P.Parser(
      input => {
        let result = P.runOnInput(parser, input)
        //        "opPayloadType0"->log

        switch result {
        | Ok((p1Result, inputAfterP1)) => {
            let (_, nBits) = p1Result
            //            `  c = ${c->charToString}`->log
            //            j`  nBits = $nBits`->log
            //            let reminderPackets = sequenceN(binDigit, nBits)->P.map(binCharListToStr)
            let reminderPackets = sequenceN_(binDigit, nBits)->P.map(binCharListToStr)
            let reminderResult = P.runOnInput(reminderPackets, inputAfterP1)
            //            j`  done running reminderpackages`->log
            switch reminderResult {
            | Ok((packetStr, inputAfterReminder)) => {
                //                j`  packetStr = $packetStr`->log

                let packetsResult = P.run(P.many(p), packetStr)

                switch packetsResult {
                | Ok((resultAfterReminder, _)) =>
                  Ok((p1Result, resultAfterReminder), inputAfterReminder)
                | Error(err) =>
                  //                    "  Error many(p)"->log
                  Error(err)
                }
              }
            | Error(err) =>
              //                "  Error reminderPackets"->log
              Error(err)
            }
          }
        | Error(err) => Error(err)
        }
      },
    )

    // opPayload Type 1  len = 11 bit (number of packets)
    //
    let opPayloadType1: opPayloadType1_<'a, 'b> = parser => P.Parser(
      input => {
        let result = P.runOnInput(parser, input)
        //        "opPayloadType1"->log
        switch result {
        | Ok((p1Result, inputAfterP1)) => {
            let (_, nPacket) = p1Result
            //            `  c = ${c->charToString}`->log

            //            j`  nPacket = $nPacket`->log

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

    let operatorPayload: literalPayload_ = {
      let lengthType_0 = P.char('0')->P.andThen(binDigits_15_int)->opPayloadType0
      let lengthType_1 = P.char('1')->P.andThen(binDigits_11_int)->opPayloadType1

      P.choice([lengthType_0, lengthType_1])->P.map((((len_type, len), rest_packets)) => {
        switch len_type {
        | '0' => Op_Len_Kind_0(len, rest_packets)
        | '1' => Op_Len_Kind_1(len, rest_packets)
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

    | Op_Len_Kind_0(len, rest) => {
        let sub_packets_str = rest_packet_str(rest)

        j`{ ver = $version | typeId = $typeId | op payload = type_0(n_bits: $len, $sub_packets_str) }\n`
      }
    | Op_Len_Kind_1(len, rest) => {
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
      | Op_Len_Kind_0(_, rest)
      | Op_Len_Kind_1(_, rest) =>
        version + List.reduce(rest, 0, (a, p) => {a + inner(p, 0)})
      }
    }

    inner(p, 0)
  }

  type result = P.parseResult<packet>
  let parse = (s): result => P.run(packet, s)
}

module Expression = {
  //  let int_max = Int64.fromString("9007199254740991")
  //  let int_min = Int64.fromString("-9007199254740991")
  let int_max = Int64.max_int
  let int_min = Int64.min_int

  type rec value<_> = Int(Int64.t): value<Int64.t>

  type rec expr<_> =
    | Value(value<'a>): expr<'a>
    | Sum(list<expr<Int64.t>>): expr<Int64.t>
    | Product(list<expr<Int64.t>>): expr<Int64.t>
    | Min(list<expr<Int64.t>>): expr<Int64.t>
    | Max(list<expr<Int64.t>>): expr<Int64.t>
    | Greater(expr<Int64.t>, expr<Int64.t>): expr<Int64.t>
    | Less(expr<Int64.t>, expr<Int64.t>): expr<Int64.t>
    | Equal(expr<Int64.t>, expr<Int64.t>): expr<Int64.t>

  let intVal = x => Value(Int(Int64.of_int(x)))

  let eval_value = (type a, v: value<a>): a => {
    switch v {
    | Int(v) => v
    }
  }
  let zero_64 = Int64.of_int(0)
  let one_64 = Int64.of_int(1)

  let rec eval:
    type a. expr<a> => a =
    (type a, e: expr<a>): a => {
      switch e {
      | Value(v) => eval_value(v)
      | Sum(e) => e->List.reduce(zero_64, (a, v) => Int64.add(a, eval(v)))
      | Product(e) => e->List.reduce(one_64, (a, v) => Int64.mul(a, eval(v)))
      | Min(e) =>
        e->List.reduce(int_max, (a, v) => {
          let v' = eval(v)
          Int64.compare(v', a) < 0 ? v' : a
        })
      | Max(e) =>
        e->List.reduce(int_min, (a, v) => {
          let v' = eval(v)
          Int64.compare(v', a) > 0 ? v' : a
        })
      | Greater(e1, e2) => Int64.compare(eval(e1), eval(e2)) > 0 ? one_64 : zero_64
      | Less(e1, e2) => Int64.compare(eval(e1), eval(e2)) < 0 ? one_64 : zero_64
      | Equal(e1, e2) => Int64.compare(eval(e1), eval(e2)) == 0 ? one_64 : zero_64
      }
    }

  let rec toString = e => {
    switch e {
    | Value(v) => {
        let v' = eval_value(v)
        j` Value=$v';`
      }
    | Sum(e) => {
        let v' = e->List.reduce("", (a, v) => a ++ toString(v))
        j`Sum:{ $v' }`
      }
    | Product(e) => {
        let v' = e->List.reduce("", (a, v) => a ++ toString(v))
        j`Product:{ $v' }`
      }
    | Min(e) => {
        let v' = e->List.reduce("", (a, v) => a ++ toString(v))
        j`Min:{ $v' }`
      }
    | Max(e) => {
        let v' = e->List.reduce("", (a, v) => a ++ toString(v))
        j`Max:{ $v' }`
      }
    | Greater(e1, e2) => {
        let v1 = toString(e1)
        let v2 = toString(e2)
        j`Greater:{ $v1, $v2 }`
      }
    | Less(e1, e2) => {
        let v1 = toString(e1)
        let v2 = toString(e2)
        j`LessThan:{ $v1, $v2 }`
      }
    | Equal(e1, e2) => {
        let v1 = toString(e1)
        let v2 = toString(e2)
        j`Equal:{ $v1, $v2 }`
      }
    }
  }

  open Packet
  let rec makeFromPacket = (p: packet) => {
    let Packet(Version(_), TypeID(typeId), p) = p

    switch p {
    | Literal(l) => Value(Int(l))
    | Op_Len_Kind_0(_, rest)
    | Op_Len_Kind_1(_, rest) =>
      switch typeId {
      | 0 => Sum(rest->List.map(makeFromPacket))
      | 1 => Product(rest->List.map(makeFromPacket))
      | 2 => Min(rest->List.map(makeFromPacket))
      | 3 => Max(rest->List.map(makeFromPacket))
      | 5 =>
        Greater(
          makeFromPacket(rest->List.headExn),
          makeFromPacket(rest->List.tailExn->List.headExn),
        )
      | 6 =>
        Less(makeFromPacket(rest->List.headExn), makeFromPacket(rest->List.tailExn->List.headExn))
      | 7 =>
        Equal(makeFromPacket(rest->List.headExn), makeFromPacket(rest->List.tailExn->List.headExn))
      | _ => raise(ParseError("Unknown typeId"))
      }
    }
  }
}

//let parse = data => data->splitNewline->Array.map(Js.String2.trim)

let solvePart1 = data => {
  let d = data->hexStrToBinStr

  let l = Packet.parse(d)
  //  l->Result.isOk->log2(d)
  let p = l->Result.getExn->fst
  switch l {
  | Ok(_) =>
    //      p->dumpPacket->log
    //      l->Result.getExn->snd->log
    //      log("\n")
    p->Packet.version_sum
  | Error(err) => {
      log(err)
      log("\n")
      0
    }
  }
}

let solvePart2 = data => {
  open Expression

  let d = data->hexStrToBinStr
  let l = Packet.parse(d)
  //  l->Result.isOk->log2(d)
  let p = l->Result.getExn->fst

  switch l {
  | Ok(_) =>
    let e = p->makeFromPacket
    //    "Expression: "->log
    //    e->toString->log->log
    //    "Eval'ed: "->log
    e->eval->Int64.to_string

  | Error(err) => {
      log(err)
      log("\n")
      0->Int64.of_int->Int64.to_string
    }
  }
}
