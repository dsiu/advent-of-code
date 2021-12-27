// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Belt_Option = require("rescript/lib/js/belt_Option.js");
var Jest2$AdventOfCode = require("../interop/Jest2.bs.js");
var Array2D$AdventOfCode = require("../src/Array2D.bs.js");
var AOC2020_Day4_Data$AdventOfCode = require("../src/2020/Day4/AOC2020_Day4_Data.bs.js");
var AOC2020_Day4_Data_Sample$AdventOfCode = require("../src/2020/Day4/AOC2020_Day4_Data_Sample.bs.js");

describe("make", (function () {
        test("make - int", (function () {
                var a = Array2D$AdventOfCode.make([
                      3,
                      2
                    ], -1);
                [
                  Array2D$AdventOfCode.set(a, [
                        0,
                        0
                      ], 4),
                  Array2D$AdventOfCode.set(a, [
                        1,
                        0
                      ], 5),
                  Array2D$AdventOfCode.set(a, [
                        2,
                        0
                      ], 6),
                  Array2D$AdventOfCode.set(a, [
                        0,
                        1
                      ], 7),
                  Array2D$AdventOfCode.set(a, [
                        1,
                        1
                      ], 8),
                  Array2D$AdventOfCode.set(a, [
                        2,
                        1
                      ], 9)
                ];
                var expected = [
                  [
                    4,
                    5,
                    6
                  ],
                  [
                    7,
                    8,
                    9
                  ]
                ];
                expect(a).toEqual(expected);
                
              }));
        test("make - string", (function () {
                var a = Array2D$AdventOfCode.make([
                      3,
                      2
                    ], "");
                [
                  Array2D$AdventOfCode.set(a, [
                        0,
                        0
                      ], "a"),
                  Array2D$AdventOfCode.set(a, [
                        1,
                        0
                      ], "b"),
                  Array2D$AdventOfCode.set(a, [
                        2,
                        0
                      ], "c"),
                  Array2D$AdventOfCode.set(a, [
                        0,
                        1
                      ], "d"),
                  Array2D$AdventOfCode.set(a, [
                        1,
                        1
                      ], "e"),
                  Array2D$AdventOfCode.set(a, [
                        2,
                        1
                      ], "f")
                ];
                var expected = [
                  [
                    "a",
                    "b",
                    "c"
                  ],
                  [
                    "d",
                    "e",
                    "f"
                  ]
                ];
                expect(a).toEqual(expected);
                
              }));
        
      }));

describe("get / set", (function () {
        var s = Array2D$AdventOfCode.make([
              2,
              2
            ], "");
        [
          Array2D$AdventOfCode.set(s, [
                0,
                0
              ], "e"),
          Array2D$AdventOfCode.set(s, [
                1,
                0
              ], "f"),
          Array2D$AdventOfCode.set(s, [
                0,
                1
              ], "g"),
          Array2D$AdventOfCode.set(s, [
                1,
                1
              ], "h")
        ];
        var get_string_tests = [
          [
            Belt_Option.getExn(Array2D$AdventOfCode.get(s, [
                      1,
                      0
                    ])),
            "f"
          ],
          [
            Belt_Option.getExn(Array2D$AdventOfCode.get(s, [
                      0,
                      1
                    ])),
            "g"
          ],
          [
            Belt_Option.getExn(Array2D$AdventOfCode.get(s, [
                      0,
                      0
                    ])),
            "e"
          ],
          [
            Belt_Option.getExn(Array2D$AdventOfCode.get(s, [
                      1,
                      1
                    ])),
            "h"
          ]
        ];
        Jest2$AdventOfCode.testEach2("get - string", get_string_tests, (function (result, expected) {
                expect(result).toEqual(expected);
                
              }));
        var a = Array2D$AdventOfCode.make([
              2,
              2
            ], -1);
        [
          Array2D$AdventOfCode.set(a, [
                0,
                0
              ], 1),
          Array2D$AdventOfCode.set(a, [
                1,
                0
              ], 2),
          Array2D$AdventOfCode.set(a, [
                0,
                1
              ], 3),
          Array2D$AdventOfCode.set(a, [
                1,
                1
              ], 4)
        ];
        var b = Array2D$AdventOfCode.copy(a);
        var get_tests = [
          [
            Array2D$AdventOfCode.get(a, [
                  0,
                  0
                ]),
            1
          ],
          [
            Array2D$AdventOfCode.get(a, [
                  1,
                  0
                ]),
            2
          ],
          [
            Array2D$AdventOfCode.get(a, [
                  0,
                  1
                ]),
            3
          ],
          [
            Array2D$AdventOfCode.get(a, [
                  1,
                  1
                ]),
            4
          ]
        ];
        Jest2$AdventOfCode.testEach2("get - int", get_tests, (function (result, expected) {
                expect(result).toEqual(expected);
                
              }));
        [
          Array2D$AdventOfCode.set(a, [
                0,
                0
              ], 5),
          Array2D$AdventOfCode.set(a, [
                0,
                1
              ], 7)
        ];
        var set_tests = [
          [
            Array2D$AdventOfCode.get(a, [
                  0,
                  0
                ]),
            5
          ],
          [
            Array2D$AdventOfCode.get(a, [
                  1,
                  0
                ]),
            2
          ],
          [
            Array2D$AdventOfCode.get(a, [
                  0,
                  1
                ]),
            7
          ],
          [
            Array2D$AdventOfCode.get(a, [
                  1,
                  1
                ]),
            4
          ]
        ];
        Jest2$AdventOfCode.testEach2("set - int", set_tests, (function (result, expected) {
                expect(result).toEqual(expected);
                
              }));
        [Array2D$AdventOfCode.setYEquals(b, 1, [
                9,
                11
              ])];
        var setYEquals_test = [
          [
            Array2D$AdventOfCode.get(b, [
                  0,
                  0
                ]),
            1
          ],
          [
            Array2D$AdventOfCode.get(b, [
                  1,
                  0
                ]),
            2
          ],
          [
            Array2D$AdventOfCode.get(b, [
                  0,
                  1
                ]),
            9
          ],
          [
            Array2D$AdventOfCode.get(b, [
                  1,
                  1
                ]),
            11
          ]
        ];
        return Jest2$AdventOfCode.testEach2("setYEquals", setYEquals_test, (function (result, expected) {
                      expect(result).toEqual(expected);
                      
                    }));
      }));

describe("setYEquals / getYEquals", (function () {
        var a = Array2D$AdventOfCode.make([
              2,
              2
            ], -1);
        [
          Array2D$AdventOfCode.set(a, [
                0,
                0
              ], 1),
          Array2D$AdventOfCode.set(a, [
                1,
                0
              ], 2),
          Array2D$AdventOfCode.set(a, [
                0,
                1
              ], 3),
          Array2D$AdventOfCode.set(a, [
                1,
                1
              ], 4)
        ];
        
      }));

describe("keep / map / getXYEquals / crop", (function () {
        var a = Array2D$AdventOfCode.make([
              3,
              4
            ], -1);
        [
          Array2D$AdventOfCode.set(a, [
                0,
                0
              ], 355),
          Array2D$AdventOfCode.set(a, [
                1,
                0
              ], 907),
          Array2D$AdventOfCode.set(a, [
                2,
                0
              ], 707),
          Array2D$AdventOfCode.set(a, [
                0,
                1
              ], 404),
          Array2D$AdventOfCode.set(a, [
                1,
                1
              ], 559),
          Array2D$AdventOfCode.set(a, [
                2,
                1
              ], 514),
          Array2D$AdventOfCode.set(a, [
                0,
                2
              ], 320),
          Array2D$AdventOfCode.set(a, [
                1,
                2
              ], 891),
          Array2D$AdventOfCode.set(a, [
                2,
                2
              ], 982),
          Array2D$AdventOfCode.set(a, [
                0,
                3
              ], 744),
          Array2D$AdventOfCode.set(a, [
                1,
                3
              ], 97),
          Array2D$AdventOfCode.set(a, [
                2,
                3
              ], 876)
        ];
        test("map - int", (function () {
                var result = Array2D$AdventOfCode.map(a, (function (x) {
                        return (x << 1);
                      }));
                var expected = [
                  [
                    710,
                    1814,
                    1414
                  ],
                  [
                    808,
                    1118,
                    1028
                  ],
                  [
                    640,
                    1782,
                    1964
                  ],
                  [
                    1488,
                    194,
                    1752
                  ]
                ];
                expect(result).toEqual(expected);
                
              }));
        var getXEquals_tests = [
          [
            Array2D$AdventOfCode.getXEquals(a, 0),
            [
              355,
              404,
              320,
              744
            ]
          ],
          [
            Array2D$AdventOfCode.getXEquals(a, 1),
            [
              907,
              559,
              891,
              97
            ]
          ],
          [
            Array2D$AdventOfCode.getXEquals(a, 2),
            [
              707,
              514,
              982,
              876
            ]
          ]
        ];
        Jest2$AdventOfCode.testEach2("getXEquals", getXEquals_tests, (function (result, expected) {
                expect(result).toEqual(expected);
                
              }));
        var getYEquals_tests = [
          [
            Array2D$AdventOfCode.getYEquals(a, 0),
            [
              355,
              907,
              707
            ]
          ],
          [
            Array2D$AdventOfCode.getYEquals(a, 1),
            [
              404,
              559,
              514
            ]
          ],
          [
            Array2D$AdventOfCode.getYEquals(a, 2),
            [
              320,
              891,
              982
            ]
          ]
        ];
        Jest2$AdventOfCode.testEach2("getYEquals", getYEquals_tests, (function (result, expected) {
                expect(result).toEqual(expected);
                
              }));
        var crop_tests = [
          [
            Array2D$AdventOfCode.crop(a, [
                  1,
                  2
                ], 1, 2),
            [
              [891],
              [97]
            ]
          ],
          [
            Array2D$AdventOfCode.crop(a, [
                  1,
                  1
                ], 2, 2),
            [
              [
                559,
                514
              ],
              [
                891,
                982
              ]
            ]
          ]
        ];
        return Jest2$AdventOfCode.testEach2("crop", crop_tests, (function (result, expected) {
                      expect(result).toEqual(expected);
                      
                    }));
      }));

var data = AOC2020_Day4_Data$AdventOfCode.data;

var testData = AOC2020_Day4_Data_Sample$AdventOfCode.data;

exports.data = data;
exports.testData = testData;
/*  Not a pure module */
