// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Int64 from "rescript/lib/es6/int64.js";
import * as Js_dict from "rescript/lib/es6/js_dict.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_int64 from "rescript/lib/es6/caml_int64.js";
import * as Res_parser from "@resinfo/parser/src/res_parser.mjs";
import * as Belt_Result from "rescript/lib/es6/belt_Result.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
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

var ParseError = /* @__PURE__ */Caml_exceptions.create("AOC2021_Day16-AdventOfCode.ParseError");

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
        return {
                TAG: /* Literal */0,
                _0: Caml_int64.of_float(Number(BigInt("0b" + (Belt_List.reduce(param[0], "", (function (a, param) {
                                      return a + param[1];
                                    })) + param[1][1]))))
              };
      }));

var remainingBinDigitStr = Res_parser.map(Res_parser.many(binDigit), stringifyCharList);

var packet = Res_parser.makeRecursive(function (p) {
      var opPayloadType0 = function (parser) {
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
                    var reminderPackets = Res_parser.map(sequenceN_(binDigit, p1Result[1]), binCharListToStr);
                    var reminderResult = Res_parser.runOnInput(reminderPackets, match[1]);
                    if (reminderResult.TAG !== /* Ok */0) {
                      return {
                              TAG: /* Error */1,
                              _0: reminderResult._0
                            };
                    }
                    var match$1 = reminderResult._0;
                    var packetsResult = Res_parser.run(Res_parser.many(p), match$1[0]);
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
                    } else {
                      return {
                              TAG: /* Error */1,
                              _0: packetsResult._0
                            };
                    }
                  })
              };
      };
      var opPayloadType1 = function (parser) {
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
                    var reminderPackets = Res_parser.sequence(Belt_List.makeBy(p1Result[1], (function (param) {
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
                        TAG: /* Op_Len_Kind_0 */1,
                        _0: len,
                        _1: rest_packets
                      };
              }
              if (len_type !== 49) {
                throw {
                      RE_EXN_ID: ParseError,
                      _1: "unknown operator len type = " + String.fromCharCode(len_type),
                      Error: new Error()
                    };
              }
              return {
                      TAG: /* Op_Len_Kind_1 */2,
                      _0: len,
                      _1: rest_packets
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
  switch (p$1.TAG | 0) {
    case /* Literal */0 :
        return "ver = " + version + " | typeId = " + typeId + " | literal payload = " + p$1._0;
    case /* Op_Len_Kind_0 */1 :
        var sub_packets_str = rest_packet_str(p$1._1);
        return "{ ver = " + version + " | typeId = " + typeId + " | op payload = type_0(n_bits: " + p$1._0 + ", " + sub_packets_str + ") }\n";
    case /* Op_Len_Kind_1 */2 :
        var sub_packets_str$1 = rest_packet_str(p$1._1);
        return "{ ver = " + version + " | typeId = " + typeId + " | op payload = type_1(n_packats: " + p$1._0 + ", " + sub_packets_str$1 + ") }\n";
    
  }
}

function version_sum(p) {
  var inner = function (p, sum) {
    var payload = p._2;
    var version = p._0._0;
    switch (payload.TAG | 0) {
      case /* Literal */0 :
          return sum + version | 0;
      case /* Op_Len_Kind_0 */1 :
      case /* Op_Len_Kind_1 */2 :
          break;
      
    }
    return version + Belt_List.reduce(payload._1, 0, (function (a, p) {
                  return a + inner(p, 0) | 0;
                })) | 0;
  };
  return inner(p, 0);
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
  version_sum: version_sum,
  parse: parse
};

function intVal(x) {
  return {
          TAG: /* Value */0,
          _0: /* Int */{
            _0: Caml_int64.of_int32(x)
          }
        };
}

function eval_value(v) {
  return v._0;
}

var zero_64 = Caml_int64.zero;

var one_64 = Caml_int64.one;

function $$eval(e) {
  switch (e.TAG | 0) {
    case /* Value */0 :
        return e._0._0;
    case /* Sum */1 :
        return Belt_List.reduce(e._0, zero_64, (function (a, v) {
                      return Caml_int64.add(a, $$eval(v));
                    }));
    case /* Product */2 :
        return Belt_List.reduce(e._0, one_64, (function (a, v) {
                      return Caml_int64.mul(a, $$eval(v));
                    }));
    case /* Min */3 :
        return Belt_List.reduce(e._0, Int64.max_int, (function (a, v) {
                      var v$p = $$eval(v);
                      if (Int64.compare(v$p, a) < 0) {
                        return v$p;
                      } else {
                        return a;
                      }
                    }));
    case /* Max */4 :
        return Belt_List.reduce(e._0, Int64.min_int, (function (a, v) {
                      var v$p = $$eval(v);
                      if (Int64.compare(v$p, a) > 0) {
                        return v$p;
                      } else {
                        return a;
                      }
                    }));
    case /* Greater */5 :
        if (Int64.compare($$eval(e._0), $$eval(e._1)) > 0) {
          return one_64;
        } else {
          return zero_64;
        }
    case /* Less */6 :
        if (Int64.compare($$eval(e._0), $$eval(e._1)) < 0) {
          return one_64;
        } else {
          return zero_64;
        }
    case /* Equal */7 :
        if (Int64.compare($$eval(e._0), $$eval(e._1)) === 0) {
          return one_64;
        } else {
          return zero_64;
        }
    
  }
}

function dump(e) {
  switch (e.TAG | 0) {
    case /* Value */0 :
        var v$p = e._0._0;
        return " Value=" + v$p + ";";
    case /* Sum */1 :
        var v$p$1 = Belt_List.reduce(e._0, "", (function (a, v) {
                return a + dump(v);
              }));
        return "Sum:{ " + v$p$1 + " }";
    case /* Product */2 :
        var v$p$2 = Belt_List.reduce(e._0, "", (function (a, v) {
                return a + dump(v);
              }));
        return "Product:{ " + v$p$2 + " }";
    case /* Min */3 :
        var v$p$3 = Belt_List.reduce(e._0, "", (function (a, v) {
                return a + dump(v);
              }));
        return "Min:{ " + v$p$3 + " }";
    case /* Max */4 :
        var v$p$4 = Belt_List.reduce(e._0, "", (function (a, v) {
                return a + dump(v);
              }));
        return "Max:{ " + v$p$4 + " }";
    case /* Greater */5 :
        var v1 = dump(e._0);
        var v2 = dump(e._1);
        return "Greater:{ " + v1 + ", " + v2 + " }";
    case /* Less */6 :
        var v1$1 = dump(e._0);
        var v2$1 = dump(e._1);
        return "LessThan:{ " + v1$1 + ", " + v2$1 + " }";
    case /* Equal */7 :
        var v1$2 = dump(e._0);
        var v2$2 = dump(e._1);
        return "Equal:{ " + v1$2 + ", " + v2$2 + " }";
    
  }
}

function makeFromPacket(p) {
  var p$1 = p._2;
  switch (p$1.TAG | 0) {
    case /* Literal */0 :
        return {
                TAG: /* Value */0,
                _0: /* Int */{
                  _0: p$1._0
                }
              };
    case /* Op_Len_Kind_0 */1 :
    case /* Op_Len_Kind_1 */2 :
        break;
    
  }
  var rest = p$1._1;
  switch (p._1._0) {
    case 0 :
        return {
                TAG: /* Sum */1,
                _0: Belt_List.map(rest, makeFromPacket)
              };
    case 1 :
        return {
                TAG: /* Product */2,
                _0: Belt_List.map(rest, makeFromPacket)
              };
    case 2 :
        return {
                TAG: /* Min */3,
                _0: Belt_List.map(rest, makeFromPacket)
              };
    case 3 :
        return {
                TAG: /* Max */4,
                _0: Belt_List.map(rest, makeFromPacket)
              };
    case 4 :
        throw {
              RE_EXN_ID: ParseError,
              _1: "Unknown typeId",
              Error: new Error()
            };
    case 5 :
        return {
                TAG: /* Greater */5,
                _0: makeFromPacket(Belt_List.headExn(rest)),
                _1: makeFromPacket(Belt_List.headExn(Belt_List.tailExn(rest)))
              };
    case 6 :
        return {
                TAG: /* Less */6,
                _0: makeFromPacket(Belt_List.headExn(rest)),
                _1: makeFromPacket(Belt_List.headExn(Belt_List.tailExn(rest)))
              };
    case 7 :
        return {
                TAG: /* Equal */7,
                _0: makeFromPacket(Belt_List.headExn(rest)),
                _1: makeFromPacket(Belt_List.headExn(Belt_List.tailExn(rest)))
              };
    default:
      throw {
            RE_EXN_ID: ParseError,
            _1: "Unknown typeId",
            Error: new Error()
          };
  }
}

var Expression = {
  int_max: Int64.max_int,
  int_min: Int64.min_int,
  intVal: intVal,
  eval_value: eval_value,
  zero_64: zero_64,
  one_64: one_64,
  $$eval: $$eval,
  dump: dump,
  makeFromPacket: makeFromPacket
};

function solvePart1(data) {
  var d = hexStrToBinStr(data);
  var l = Res_parser.run(packet, d);
  var p = Belt_Result.getExn(l)[0];
  if (l.TAG === /* Ok */0) {
    return version_sum(p);
  }
  console.log(l._0);
  console.log("\n");
  return 0;
}

function solvePart2(data) {
  var d = hexStrToBinStr(data);
  var l = Res_parser.run(packet, d);
  var p = Belt_Result.getExn(l)[0];
  if (l.TAG === /* Ok */0) {
    var e = makeFromPacket(p);
    return Int64.to_string($$eval(e));
  }
  console.log(l._0);
  console.log("\n");
  return Int64.to_string(Caml_int64.zero);
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
  ParseError ,
  Packet ,
  Expression ,
  solvePart1 ,
  solvePart2 ,
  
}
/* hexTable Not a pure module */
