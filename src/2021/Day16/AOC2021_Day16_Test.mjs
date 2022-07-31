// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Res_parser from "@resinfo/parser/src/res_parser.mjs";
import * as Belt_Result from "rescript/lib/es6/belt_Result.js";
import * as Jest2$AdventOfCode from "../../../interop/Jest2.mjs";
import * as AOC2021_Day16$AdventOfCode from "./AOC2021_Day16.mjs";
import * as AOC2021_Day16_Data$AdventOfCode from "./AOC2021_Day16_Data.mjs";
import * as AOC2021_Day16_Data_Sample$AdventOfCode from "./AOC2021_Day16_Data_Sample.mjs";

function literal(ver, l) {
  return /* Packet */{
          _0: /* Version */{
            _0: ver
          },
          _1: /* TypeID */{
            _0: 4
          },
          _2: {
            TAG: /* Literal */0,
            _0: l
          }
        };
}

function op_type_0(ver, t, param) {
  return /* Packet */{
          _0: /* Version */{
            _0: ver
          },
          _1: /* TypeID */{
            _0: t
          },
          _2: {
            TAG: /* Op_Len_Kind_0 */1,
            _0: param[0],
            _1: param[1]
          }
        };
}

function op_type_1(ver, t, param) {
  return /* Packet */{
          _0: /* Version */{
            _0: ver
          },
          _1: /* TypeID */{
            _0: t
          },
          _2: {
            TAG: /* Op_Len_Kind_1 */2,
            _0: param[0],
            _1: param[1]
          }
        };
}

var anyChar = Res_parser.satisfy(function (param) {
      return true;
    });

describe("2021 Day16", (function () {
        describe("Parser Utils", (function () {
                test("hex string to binary", (function () {
                        var result = AOC2021_Day16$AdventOfCode.hexStrToBinStr("D2FE28");
                        expect(result).toEqual("110100101111111000101000");
                        
                      }));
                test("sequenceN", (function () {
                        var p = Res_parser.map(AOC2021_Day16$AdventOfCode.Packet.sequenceN(anyChar, 6), AOC2021_Day16$AdventOfCode.binCharListToStr);
                        var result = Belt_Result.getExn(Res_parser.run(p, "123456"))[0];
                        expect(result).toEqual("123456");
                        
                      }));
                test("sequenceN_", (function () {
                        var p = Res_parser.map(AOC2021_Day16$AdventOfCode.Packet.sequenceN_(anyChar, 6), AOC2021_Day16$AdventOfCode.binCharListToStr);
                        var result = Belt_Result.getExn(Res_parser.run(p, "123456"))[0];
                        expect(result).toEqual("123456");
                        
                      }));
                
              }));
        describe("Packet Parser", (function () {
                test("literal 2021", (function () {
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse("110100101111111000101000"))[0];
                        var expected_0 = /* Version */{
                          _0: 6
                        };
                        var expected_1 = /* TypeID */{
                          _0: 4
                        };
                        var expected_2 = {
                          TAG: /* Literal */0,
                          _0: 2021
                        };
                        var expected = /* Packet */{
                          _0: expected_0,
                          _1: expected_1,
                          _2: expected_2
                        };
                        expect(result).toEqual(expected);
                        
                      }));
                test("literal 10", (function () {
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse("11010001010"))[0];
                        var expected_0 = /* Version */{
                          _0: 6
                        };
                        var expected_1 = /* TypeID */{
                          _0: 4
                        };
                        var expected_2 = {
                          TAG: /* Literal */0,
                          _0: 10
                        };
                        var expected = /* Packet */{
                          _0: expected_0,
                          _1: expected_1,
                          _2: expected_2
                        };
                        expect(result).toEqual(expected);
                        
                      }));
                test("literal 20", (function () {
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse("0101001000100100"))[0];
                        var expected_0 = /* Version */{
                          _0: 2
                        };
                        var expected_1 = /* TypeID */{
                          _0: 4
                        };
                        var expected_2 = {
                          TAG: /* Literal */0,
                          _0: 20
                        };
                        var expected = /* Packet */{
                          _0: expected_0,
                          _1: expected_1,
                          _2: expected_2
                        };
                        expect(result).toEqual(expected);
                        
                      }));
                test("op type 0 simple", (function () {
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse("00111000000000000110111101000101001010010001001000000000"))[0];
                        var expected = op_type_0(1, 6, [
                              27,
                              {
                                hd: /* Packet */{
                                  _0: /* Version */{
                                    _0: 6
                                  },
                                  _1: /* TypeID */{
                                    _0: 4
                                  },
                                  _2: {
                                    TAG: /* Literal */0,
                                    _0: 10
                                  }
                                },
                                tl: {
                                  hd: /* Packet */{
                                    _0: /* Version */{
                                      _0: 2
                                    },
                                    _1: /* TypeID */{
                                      _0: 4
                                    },
                                    _2: {
                                      TAG: /* Literal */0,
                                      _0: 20
                                    }
                                  },
                                  tl: /* [] */0
                                }
                              }
                            ]);
                        expect(result).toEqual(expected);
                        
                      }));
                test("op type 1 simple", (function () {
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse("11101110000000001101010000001100100000100011000001100000"))[0];
                        var expected = op_type_1(7, 3, [
                              3,
                              {
                                hd: /* Packet */{
                                  _0: /* Version */{
                                    _0: 2
                                  },
                                  _1: /* TypeID */{
                                    _0: 4
                                  },
                                  _2: {
                                    TAG: /* Literal */0,
                                    _0: 1
                                  }
                                },
                                tl: {
                                  hd: /* Packet */{
                                    _0: /* Version */{
                                      _0: 4
                                    },
                                    _1: /* TypeID */{
                                      _0: 4
                                    },
                                    _2: {
                                      TAG: /* Literal */0,
                                      _0: 2
                                    }
                                  },
                                  tl: {
                                    hd: /* Packet */{
                                      _0: /* Version */{
                                        _0: 1
                                      },
                                      _1: /* TypeID */{
                                        _0: 4
                                      },
                                      _2: {
                                        TAG: /* Literal */0,
                                        _0: 3
                                      }
                                    },
                                    tl: /* [] */0
                                  }
                                }
                              }
                            ]);
                        expect(result).toEqual(expected);
                        
                      }));
                test("example 1", (function () {
                        var input = AOC2021_Day16$AdventOfCode.hexStrToBinStr("8A004A801A8002F478");
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse(input))[0];
                        var versionSum = AOC2021_Day16$AdventOfCode.Packet.version_sum(result);
                        var expected = op_type_1(4, 2, [
                              1,
                              {
                                hd: op_type_1(1, 2, [
                                      1,
                                      {
                                        hd: op_type_0(5, 2, [
                                              11,
                                              {
                                                hd: /* Packet */{
                                                  _0: /* Version */{
                                                    _0: 6
                                                  },
                                                  _1: /* TypeID */{
                                                    _0: 4
                                                  },
                                                  _2: {
                                                    TAG: /* Literal */0,
                                                    _0: 15
                                                  }
                                                },
                                                tl: /* [] */0
                                              }
                                            ]),
                                        tl: /* [] */0
                                      }
                                    ]),
                                tl: /* [] */0
                              }
                            ]);
                        expect([
                                result,
                                versionSum
                              ]).toEqual([
                              expected,
                              16
                            ]);
                        
                      }));
                test("example 2", (function () {
                        var input = AOC2021_Day16$AdventOfCode.hexStrToBinStr("620080001611562C8802118E34");
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse(input))[0];
                        var versionSum = AOC2021_Day16$AdventOfCode.Packet.version_sum(result);
                        var expected = op_type_1(3, 0, [
                              2,
                              {
                                hd: op_type_0(0, 0, [
                                      22,
                                      {
                                        hd: /* Packet */{
                                          _0: /* Version */{
                                            _0: 0
                                          },
                                          _1: /* TypeID */{
                                            _0: 4
                                          },
                                          _2: {
                                            TAG: /* Literal */0,
                                            _0: 10
                                          }
                                        },
                                        tl: {
                                          hd: /* Packet */{
                                            _0: /* Version */{
                                              _0: 5
                                            },
                                            _1: /* TypeID */{
                                              _0: 4
                                            },
                                            _2: {
                                              TAG: /* Literal */0,
                                              _0: 11
                                            }
                                          },
                                          tl: /* [] */0
                                        }
                                      }
                                    ]),
                                tl: {
                                  hd: op_type_1(1, 0, [
                                        2,
                                        {
                                          hd: /* Packet */{
                                            _0: /* Version */{
                                              _0: 0
                                            },
                                            _1: /* TypeID */{
                                              _0: 4
                                            },
                                            _2: {
                                              TAG: /* Literal */0,
                                              _0: 12
                                            }
                                          },
                                          tl: {
                                            hd: /* Packet */{
                                              _0: /* Version */{
                                                _0: 3
                                              },
                                              _1: /* TypeID */{
                                                _0: 4
                                              },
                                              _2: {
                                                TAG: /* Literal */0,
                                                _0: 13
                                              }
                                            },
                                            tl: /* [] */0
                                          }
                                        }
                                      ]),
                                  tl: /* [] */0
                                }
                              }
                            ]);
                        expect([
                                result,
                                versionSum
                              ]).toEqual([
                              expected,
                              12
                            ]);
                        
                      }));
                test("example 3", (function () {
                        var input = AOC2021_Day16$AdventOfCode.hexStrToBinStr("C0015000016115A2E0802F182340");
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse(input))[0];
                        var versionSum = AOC2021_Day16$AdventOfCode.Packet.version_sum(result);
                        var expected = op_type_0(6, 0, [
                              84,
                              {
                                hd: op_type_0(0, 0, [
                                      22,
                                      {
                                        hd: /* Packet */{
                                          _0: /* Version */{
                                            _0: 0
                                          },
                                          _1: /* TypeID */{
                                            _0: 4
                                          },
                                          _2: {
                                            TAG: /* Literal */0,
                                            _0: 10
                                          }
                                        },
                                        tl: {
                                          hd: /* Packet */{
                                            _0: /* Version */{
                                              _0: 6
                                            },
                                            _1: /* TypeID */{
                                              _0: 4
                                            },
                                            _2: {
                                              TAG: /* Literal */0,
                                              _0: 11
                                            }
                                          },
                                          tl: /* [] */0
                                        }
                                      }
                                    ]),
                                tl: {
                                  hd: op_type_1(4, 0, [
                                        2,
                                        {
                                          hd: /* Packet */{
                                            _0: /* Version */{
                                              _0: 7
                                            },
                                            _1: /* TypeID */{
                                              _0: 4
                                            },
                                            _2: {
                                              TAG: /* Literal */0,
                                              _0: 12
                                            }
                                          },
                                          tl: {
                                            hd: /* Packet */{
                                              _0: /* Version */{
                                                _0: 0
                                              },
                                              _1: /* TypeID */{
                                                _0: 4
                                              },
                                              _2: {
                                                TAG: /* Literal */0,
                                                _0: 13
                                              }
                                            },
                                            tl: /* [] */0
                                          }
                                        }
                                      ]),
                                  tl: /* [] */0
                                }
                              }
                            ]);
                        expect([
                                result,
                                versionSum
                              ]).toEqual([
                              expected,
                              23
                            ]);
                        
                      }));
                test("example 4", (function () {
                        var input = AOC2021_Day16$AdventOfCode.hexStrToBinStr("A0016C880162017C3686B18A3D4780");
                        var result = Belt_Result.getExn(AOC2021_Day16$AdventOfCode.Packet.parse(input))[0];
                        var versionSum = AOC2021_Day16$AdventOfCode.Packet.version_sum(result);
                        var expected = op_type_0(5, 0, [
                              91,
                              {
                                hd: op_type_1(1, 0, [
                                      1,
                                      {
                                        hd: op_type_1(3, 0, [
                                              5,
                                              {
                                                hd: /* Packet */{
                                                  _0: /* Version */{
                                                    _0: 7
                                                  },
                                                  _1: /* TypeID */{
                                                    _0: 4
                                                  },
                                                  _2: {
                                                    TAG: /* Literal */0,
                                                    _0: 6
                                                  }
                                                },
                                                tl: {
                                                  hd: /* Packet */{
                                                    _0: /* Version */{
                                                      _0: 6
                                                    },
                                                    _1: /* TypeID */{
                                                      _0: 4
                                                    },
                                                    _2: {
                                                      TAG: /* Literal */0,
                                                      _0: 6
                                                    }
                                                  },
                                                  tl: {
                                                    hd: /* Packet */{
                                                      _0: /* Version */{
                                                        _0: 5
                                                      },
                                                      _1: /* TypeID */{
                                                        _0: 4
                                                      },
                                                      _2: {
                                                        TAG: /* Literal */0,
                                                        _0: 12
                                                      }
                                                    },
                                                    tl: {
                                                      hd: /* Packet */{
                                                        _0: /* Version */{
                                                          _0: 2
                                                        },
                                                        _1: /* TypeID */{
                                                          _0: 4
                                                        },
                                                        _2: {
                                                          TAG: /* Literal */0,
                                                          _0: 15
                                                        }
                                                      },
                                                      tl: {
                                                        hd: /* Packet */{
                                                          _0: /* Version */{
                                                            _0: 2
                                                          },
                                                          _1: /* TypeID */{
                                                            _0: 4
                                                          },
                                                          _2: {
                                                            TAG: /* Literal */0,
                                                            _0: 15
                                                          }
                                                        },
                                                        tl: /* [] */0
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            ]),
                                        tl: /* [] */0
                                      }
                                    ]),
                                tl: /* [] */0
                              }
                            ]);
                        expect([
                                result,
                                versionSum
                              ]).toEqual([
                              expected,
                              31
                            ]);
                        
                      }));
                
              }));
        describe("Expression", (function () {
                var sum_tests = [
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Sum */1,
                          _0: {
                            hd: AOC2021_Day16$AdventOfCode.Expression.intVal(1),
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(2),
                              tl: /* [] */0
                            }
                          }
                        }),
                    3
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Sum */1,
                          _0: {
                            hd: {
                              TAG: /* Sum */1,
                              _0: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(3),
                                tl: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(4),
                                  tl: /* [] */0
                                }
                              }
                            },
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(2),
                              tl: /* [] */0
                            }
                          }
                        }),
                    9
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Sum */1,
                          _0: {
                            hd: AOC2021_Day16$AdventOfCode.Expression.intVal(11),
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(12),
                              tl: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(13),
                                tl: /* [] */0
                              }
                            }
                          }
                        }),
                    36
                  ]
                ];
                Jest2$AdventOfCode.testEach2("sum", sum_tests, (function (result, expected) {
                        expect(result).toEqual(expected);
                        
                      }));
                var product_tests = [
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Product */2,
                          _0: {
                            hd: AOC2021_Day16$AdventOfCode.Expression.intVal(2),
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(3),
                              tl: /* [] */0
                            }
                          }
                        }),
                    6
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Product */2,
                          _0: {
                            hd: {
                              TAG: /* Product */2,
                              _0: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(3),
                                tl: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(4),
                                  tl: /* [] */0
                                }
                              }
                            },
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(2),
                              tl: /* [] */0
                            }
                          }
                        }),
                    24
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Product */2,
                          _0: {
                            hd: AOC2021_Day16$AdventOfCode.Expression.intVal(11),
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(12),
                              tl: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(13),
                                tl: /* [] */0
                              }
                            }
                          }
                        }),
                    1716
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Product */2,
                          _0: {
                            hd: {
                              TAG: /* Sum */1,
                              _0: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(3),
                                tl: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(4),
                                  tl: /* [] */0
                                }
                              }
                            },
                            tl: {
                              hd: {
                                TAG: /* Sum */1,
                                _0: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(2),
                                  tl: {
                                    hd: AOC2021_Day16$AdventOfCode.Expression.intVal(1),
                                    tl: /* [] */0
                                  }
                                }
                              },
                              tl: /* [] */0
                            }
                          }
                        }),
                    21
                  ]
                ];
                Jest2$AdventOfCode.testEach2("product", product_tests, (function (result, expected) {
                        expect(result).toEqual(expected);
                        
                      }));
                var min_tests = [
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Min */3,
                          _0: {
                            hd: AOC2021_Day16$AdventOfCode.Expression.intVal(2),
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(3),
                              tl: /* [] */0
                            }
                          }
                        }),
                    2
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Min */3,
                          _0: {
                            hd: AOC2021_Day16$AdventOfCode.Expression.intVal(9),
                            tl: {
                              hd: AOC2021_Day16$AdventOfCode.Expression.intVal(-1),
                              tl: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(12),
                                tl: /* [] */0
                              }
                            }
                          }
                        }),
                    -1
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Min */3,
                          _0: {
                            hd: {
                              TAG: /* Min */3,
                              _0: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(4),
                                tl: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(5),
                                  tl: /* [] */0
                                }
                              }
                            },
                            tl: {
                              hd: {
                                TAG: /* Min */3,
                                _0: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(6),
                                  tl: {
                                    hd: AOC2021_Day16$AdventOfCode.Expression.intVal(7),
                                    tl: /* [] */0
                                  }
                                }
                              },
                              tl: /* [] */0
                            }
                          }
                        }),
                    4
                  ],
                  [
                    AOC2021_Day16$AdventOfCode.Expression.$$eval({
                          TAG: /* Min */3,
                          _0: {
                            hd: {
                              TAG: /* Sum */1,
                              _0: {
                                hd: AOC2021_Day16$AdventOfCode.Expression.intVal(3),
                                tl: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(4),
                                  tl: /* [] */0
                                }
                              }
                            },
                            tl: {
                              hd: {
                                TAG: /* Sum */1,
                                _0: {
                                  hd: AOC2021_Day16$AdventOfCode.Expression.intVal(0),
                                  tl: {
                                    hd: AOC2021_Day16$AdventOfCode.Expression.intVal(7),
                                    tl: /* [] */0
                                  }
                                }
                              },
                              tl: /* [] */0
                            }
                          }
                        }),
                    7
                  ]
                ];
                return Jest2$AdventOfCode.testEach2("min", min_tests, (function (result, expected) {
                              expect(result).toEqual(expected);
                              
                            }));
              }));
        test("Part 1 - Sample Data", (function () {
                var result = AOC2021_Day16$AdventOfCode.solvePart1(AOC2021_Day16_Data_Sample$AdventOfCode.data);
                expect(result).toEqual(31);
                
              }));
        test("Part 1 - Solve", (function () {
                var result = AOC2021_Day16$AdventOfCode.solvePart1(AOC2021_Day16_Data$AdventOfCode.data);
                expect(result).toEqual(1012);
                
              }));
        test("Part 2 - Sample Data", (function () {
                var result = AOC2021_Day16$AdventOfCode.solvePart2(AOC2021_Day16_Data_Sample$AdventOfCode.data);
                expect(result).toEqual(2);
                
              }));
        test("Part 2 - Solve", (function () {
                var result = AOC2021_Day16$AdventOfCode.solvePart2(AOC2021_Day16_Data$AdventOfCode.data);
                expect(result).toEqual(2);
                
              }));
        
      }));

var data = AOC2021_Day16_Data$AdventOfCode.data;

var sampleData = AOC2021_Day16_Data_Sample$AdventOfCode.data;

var P;

var Pac;

export {
  data ,
  sampleData ,
  P ,
  Pac ,
  literal ,
  op_type_0 ,
  op_type_1 ,
  anyChar ,
  
}
/* anyChar Not a pure module */
