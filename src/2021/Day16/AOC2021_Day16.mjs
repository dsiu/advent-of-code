// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Res_parser from "@resinfo/parser/src/res_parser.mjs";
import * as Belt_Result from "rescript/lib/es6/belt_Result.js";
import * as Utils$AdventOfCode from "../../Utils.mjs";

function log(prim) {
  console.log(prim);
  
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
  
}

function log3(prim0, prim1, prim2) {
  console.log(prim0, prim1, prim2);
  
}

var hexTable = Js_dict.fromList({
      hd: [
        "0",
        "0000"
      ],
      tl: {
        hd: [
          "1",
          "0001"
        ],
        tl: {
          hd: [
            "2",
            "0010"
          ],
          tl: {
            hd: [
              "3",
              "0011"
            ],
            tl: {
              hd: [
                "4",
                "0100"
              ],
              tl: {
                hd: [
                  "5",
                  "0101"
                ],
                tl: {
                  hd: [
                    "6",
                    "0110"
                  ],
                  tl: {
                    hd: [
                      "7",
                      "0111"
                    ],
                    tl: {
                      hd: [
                        "8",
                        "1000"
                      ],
                      tl: {
                        hd: [
                          "9",
                          "1001"
                        ],
                        tl: {
                          hd: [
                            "A",
                            "1010"
                          ],
                          tl: {
                            hd: [
                              "B",
                              "1011"
                            ],
                            tl: {
                              hd: [
                                "C",
                                "1100"
                              ],
                              tl: {
                                hd: [
                                  "D",
                                  "1101"
                                ],
                                tl: {
                                  hd: [
                                    "E",
                                    "1110"
                                  ],
                                  tl: {
                                    hd: [
                                      "F",
                                      "1111"
                                    ],
                                    tl: /* [] */0
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    });

function hexStrToBinStr(s) {
  return Belt_Array.reduce(Utils$AdventOfCode.splitChars(s), "", (function (acc, c) {
                return acc + hexTable[c];
              }));
}

function concatStringList(chars) {
  if (chars) {
    return chars.hd + concatStringList(chars.tl);
  } else {
    return "";
  }
}

function charToString(c) {
  return String.fromCharCode(c);
}

function stringifyCharList(chars) {
  return concatStringList(Belt_List.map(chars, charToString));
}

function binCharArrayToStr(xs) {
  return Belt_Array.reduce(xs, "", (function (a, v) {
                return a + String.fromCharCode(v);
              }));
}

function binCharArrayToInt(xs) {
  return parseInt(binCharArrayToStr(xs), 2);
}

function binCharListToStr(xs) {
  return Belt_List.reduce(xs, "", (function (a, v) {
                return a + String.fromCharCode(v);
              }));
}

function binCharListToInt(xs) {
  return parseInt(binCharListToStr(xs), 2);
}

var binDigit = Res_parser.satisfy(function (c) {
      if (c === /* '0' */48) {
        return true;
      } else {
        return c === /* '1' */49;
      }
    });

function sequenceN(parser, n) {
  return Res_parser.sequence(Belt_List.makeBy(n, (function (param) {
                    return parser;
                  })));
}

function sequenceN_(parser, n) {
  return /* Parser */{
          _0: (function (input) {
              var _i = n;
              var _input = input;
              var _result = /* [] */0;
              while(true) {
                var result = _result;
                var input$1 = _input;
                var i = _i;
                if (i <= 0) {
                  return {
                          TAG: /* Ok */0,
                          _0: [
                            Belt_List.reverse(result),
                            input$1
                          ]
                        };
                }
                var msg = Res_parser.runOnInput(parser, input$1);
                if (msg.TAG !== /* Ok */0) {
                  return {
                          TAG: /* Error */1,
                          _0: msg._0
                        };
                }
                var match = msg._0;
                _result = {
                  hd: match[0],
                  tl: result
                };
                _input = match[1];
                _i = i - 1 | 0;
                continue ;
              };
            })
        };
}

var binDigits_3 = sequenceN_(binDigit, 3);

var binDigits_3_int = Res_parser.map(binDigits_3, binCharListToInt);

var binDigits_3_str = Res_parser.map(binDigits_3, binCharListToStr);

var binDigits_4 = sequenceN_(binDigit, 4);

var binDigits_4_int = Res_parser.map(binDigits_4, binCharListToInt);

var binDigits_4_str = Res_parser.map(binDigits_4, binCharListToStr);

var binDigits_15 = sequenceN_(binDigit, 15);

var binDigits_15_int = Res_parser.map(binDigits_15, binCharListToInt);

var binDigits_15_str = Res_parser.map(binDigits_15, binCharListToStr);

var binDigits_11 = sequenceN_(binDigit, 11);

var binDigits_11_int = Res_parser.map(binDigits_11, binCharListToInt);

var binDigits_11_str = Res_parser.map(binDigits_11, binCharListToStr);

var version = Res_parser.map(binDigits_3_int, (function (x) {
        return /* Version */{
                _0: x
              };
      }));

var typeId = Res_parser.map(binDigits_3_int, (function (x) {
        return /* TypeID */{
                _0: x
              };
      }));

var literalTypeId = Res_parser.map(Res_parser.andThen(Res_parser.andThen(Res_parser.$$char(/* '1' */49), Res_parser.$$char(/* '0' */48)), Res_parser.$$char(/* '0' */48)), (function (param) {
        var match = param[0];
        return /* TypeID */{
                _0: parseInt(binCharArrayToStr([
                          match[0],
                          match[1],
                          param[1]
                        ]), 2)
              };
      }));

function restOfMultipleOf4Bits(parser) {
  return /* Parser */{
          _0: (function (input) {
              var result = Res_parser.runOnInput(parser, input);
              if (result.TAG !== /* Ok */0) {
                return {
                        TAG: /* Error */1,
                        _0: result._0
                      };
              }
              var match = result._0;
              var p1Result = match[0];
              var len = (Belt_List.reduce(p1Result[0], 0, (function (a, param) {
                        return (a + param[1].length | 0) + 1 | 0;
                      })) + p1Result[1][1].length | 0) + 1 | 0;
              var reminder = len % 4;
              console.log("  reminder = " + reminder + " | len = " + len);
              var reminderP = Res_parser.map(Res_parser.sequence(Belt_List.makeBy(reminder, (function (param) {
                              return binDigit;
                            }))), binCharListToStr);
              var reminderResult = Res_parser.runOnInput(reminderP, match[1]);
              if (reminderResult.TAG !== /* Ok */0) {
                return {
                        TAG: /* Error */1,
                        _0: reminderResult._0
                      };
              }
              var match$1 = reminderResult._0;
              return {
                      TAG: /* Ok */0,
                      _0: [
                        [
                          p1Result,
                          match$1[0]
                        ],
                        match$1[1]
                      ]
                    };
            })
        };
}

var oneAndFourBit = Res_parser.andThen(Res_parser.$$char(/* '1' */49), binDigits_4_str);

var zeroAndFourBit = Res_parser.andThen(Res_parser.$$char(/* '0' */48), binDigits_4_str);

var literal_payload = Res_parser.andThen(Res_parser.many(oneAndFourBit), zeroAndFourBit);

var literalPayload = Res_parser.map(literal_payload, (function (param) {
        console.log("literalPayload");
        var l = parseInt(Belt_List.reduce(param[0], "", (function (a, param) {
                    return a + param[1];
                  })) + param[1][1], 2);
        return {
                TAG: /* Payload_Literal */0,
                _0: (console.log("  l = " + l), l)
              };
      }));

var remainingBinDigitStr = Res_parser.map(Res_parser.many(binDigit), stringifyCharList);

var packet = Res_parser.makeRecursive(function (p) {
      var opPayloadType0 = function (parser) {
        return /* Parser */{
                _0: (function (input) {
                    var result = Res_parser.runOnInput(parser, input);
                    console.log("opPayloadType0");
                    if (result.TAG !== /* Ok */0) {
                      return {
                              TAG: /* Error */1,
                              _0: result._0
                            };
                    }
                    var match = result._0;
                    var p1Result = match[0];
                    var nBits = p1Result[1];
                    var prim = "  c = " + String.fromCharCode(p1Result[0]);
                    console.log(prim);
                    console.log("  nBits = " + nBits);
                    var reminderPackets = Res_parser.map(sequenceN_(binDigit, nBits), binCharListToStr);
                    var reminderResult = Res_parser.runOnInput(reminderPackets, match[1]);
                    console.log("  done running reminderpackages");
                    if (reminderResult.TAG === /* Ok */0) {
                      var match$1 = reminderResult._0;
                      var packetStr = match$1[0];
                      console.log("  packetStr = " + packetStr);
                      var packetsResult = Res_parser.run(Res_parser.many(p), packetStr);
                      if (packetsResult.TAG === /* Ok */0) {
                        return {
                                TAG: /* Ok */0,
                                _0: [
                                  [
                                    p1Result,
                                    packetsResult._0[0]
                                  ],
                                  match$1[1]
                                ]
                              };
                      }
                      console.log("  Error many(p)");
                      return {
                              TAG: /* Error */1,
                              _0: packetsResult._0
                            };
                    }
                    console.log("  Error reminderPackets");
                    return {
                            TAG: /* Error */1,
                            _0: reminderResult._0
                          };
                  })
              };
      };
      var opPayloadType1 = function (parser) {
        return /* Parser */{
                _0: (function (input) {
                    var result = Res_parser.runOnInput(parser, input);
                    console.log("opPayloadType1");
                    if (result.TAG !== /* Ok */0) {
                      return {
                              TAG: /* Error */1,
                              _0: result._0
                            };
                    }
                    var match = result._0;
                    var p1Result = match[0];
                    var nPacket = p1Result[1];
                    var prim = "  c = " + String.fromCharCode(p1Result[0]);
                    console.log(prim);
                    console.log("  nPacket = " + nPacket);
                    var reminderPackets = Res_parser.sequence(Belt_List.makeBy(nPacket, (function (param) {
                                return p;
                              })));
                    var reminderPacketsResult = Res_parser.runOnInput(reminderPackets, match[1]);
                    if (reminderPacketsResult.TAG !== /* Ok */0) {
                      return {
                              TAG: /* Error */1,
                              _0: reminderPacketsResult._0
                            };
                    }
                    var match$1 = reminderPacketsResult._0;
                    return {
                            TAG: /* Ok */0,
                            _0: [
                              [
                                p1Result,
                                match$1[0]
                              ],
                              match$1[1]
                            ]
                          };
                  })
              };
      };
      var lengthType_0 = opPayloadType0(Res_parser.andThen(Res_parser.$$char(/* '0' */48), binDigits_15_int));
      var lengthType_1 = opPayloadType1(Res_parser.andThen(Res_parser.$$char(/* '1' */49), binDigits_11_int));
      var operatorPayload = Res_parser.map(Res_parser.choice([
                lengthType_0,
                lengthType_1
              ]), (function (param) {
              var rest_packets = param[1];
              var match = param[0];
              var len = match[1];
              var len_type = match[0];
              if (len_type === 48) {
                return {
                        TAG: /* Payload_Operator */1,
                        _0: {
                          TAG: /* Operator_Type_0 */0,
                          _0: len,
                          _1: rest_packets
                        }
                      };
              }
              if (len_type !== 49) {
                throw {
                      RE_EXN_ID: "Match_failure",
                      _1: [
                        "AOC2021_Day16.res",
                        283,
                        8
                      ],
                      Error: new Error()
                    };
              }
              return {
                      TAG: /* Payload_Operator */1,
                      _0: {
                        TAG: /* Operator_Type_1 */1,
                        _0: len,
                        _1: rest_packets
                      }
                    };
            }));
      var literalAndPayload = Res_parser.andThen(literalTypeId, literalPayload);
      var operatorAndPayload = Res_parser.andThen(typeId, operatorPayload);
      return Res_parser.map(Res_parser.andThen(version, Res_parser.choice([
                          literalAndPayload,
                          operatorAndPayload
                        ])), (function (param) {
                    var match = param[1];
                    return /* Packet */{
                            _0: param[0],
                            _1: match[0],
                            _2: match[1]
                          };
                  }));
    });

function dumpPacket(p) {
  var p$1 = p._2;
  var typeId = p._1._0;
  var version = p._0._0;
  var rest_packet_str = function (rest) {
    return Belt_List.reduce(rest, "", (function (a, p) {
                  return a + "\n    " + dumpPacket(p);
                }));
  };
  if (p$1.TAG === /* Payload_Literal */0) {
    return "ver = " + version + " | typeId = " + typeId + " | literal payload = " + p$1._0;
  }
  var o = p$1._0;
  if (o.TAG === /* Operator_Type_0 */0) {
    var sub_packets_str = rest_packet_str(o._1);
    return "{ ver = " + version + " | typeId = " + typeId + " | op payload = type_0(n_bits: " + o._0 + ", " + sub_packets_str + ") }\n";
  }
  var sub_packets_str$1 = rest_packet_str(o._1);
  return "{ ver = " + version + " | typeId = " + typeId + " | op payload = type_1(n_packats: " + o._0 + ", " + sub_packets_str$1 + ") }\n";
}

function parse(s) {
  return Res_parser.run(packet, s);
}

var Packet = {
  binDigit: binDigit,
  sequenceN: sequenceN,
  sequenceN_: sequenceN_,
  binDigits_3: binDigits_3,
  binDigits_3_int: binDigits_3_int,
  binDigits_3_str: binDigits_3_str,
  binDigits_4: binDigits_4,
  binDigits_4_int: binDigits_4_int,
  binDigits_4_str: binDigits_4_str,
  binDigits_15: binDigits_15,
  binDigits_15_int: binDigits_15_int,
  binDigits_15_str: binDigits_15_str,
  binDigits_11: binDigits_11,
  binDigits_11_int: binDigits_11_int,
  binDigits_11_str: binDigits_11_str,
  version: version,
  typeId: typeId,
  operatorTypeId: typeId,
  literalTypeId: literalTypeId,
  restOfMultipleOf4Bits: restOfMultipleOf4Bits,
  literalPayload: literalPayload,
  remainingBinDigitStr: remainingBinDigitStr,
  packet: packet,
  dumpPacket: dumpPacket,
  parse: parse
};

function parse$1(data) {
  return Belt_Array.map(Utils$AdventOfCode.splitNewline(data), (function (prim) {
                return prim.trim();
              }));
}

function solvePart1(data) {
  var data_hex = Belt_Array.map([
        "C0015000016115A2E0802F182340",
        "A0016C880162017C3686B18A3D4780"
      ], hexStrToBinStr);
  Belt_Array.forEach(data_hex, (function (d) {
          var l = Res_parser.run(packet, d);
          var prim0 = Belt_Result.isOk(l);
          console.log(prim0, d);
          if (l.TAG === /* Ok */0) {
            var prim = dumpPacket(Belt_Result.getExn(l)[0]);
            console.log(prim);
            var prim$1 = Belt_Result.getExn(l)[1];
            console.log(prim$1);
            console.log("\n");
            return ;
          }
          console.log(l._0);
          console.log("\n");
          
        }));
  return 1;
}

function solvePart2(data) {
  return 2;
}

var P;

var Rjs;

export {
  log ,
  log2 ,
  log3 ,
  P ,
  Rjs ,
  hexTable ,
  hexStrToBinStr ,
  concatStringList ,
  charToString ,
  stringifyCharList ,
  binCharArrayToStr ,
  binCharArrayToInt ,
  binCharListToStr ,
  binCharListToInt ,
  Packet ,
  parse$1 as parse,
  solvePart1 ,
  solvePart2 ,
  
}
/* hexTable Not a pure module */
