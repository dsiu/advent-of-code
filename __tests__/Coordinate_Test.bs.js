// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest2$AdventOfCode = require("../interop/Jest2.bs.js");
var Coordinate$AdventOfCode = require("../src/Coordinate.bs.js");

describe("Step Functions", (function () {
        var init = [
          4,
          4
        ];
        var singleStep_tests = [
          [
            Coordinate$AdventOfCode.stepN(init),
            [
              4,
              3
            ]
          ],
          [
            Coordinate$AdventOfCode.stepE(init),
            [
              5,
              4
            ]
          ],
          [
            Coordinate$AdventOfCode.stepS(init),
            [
              4,
              5
            ]
          ],
          [
            Coordinate$AdventOfCode.stepW(init),
            [
              3,
              4
            ]
          ],
          [
            Coordinate$AdventOfCode.stepNE(init),
            [
              5,
              3
            ]
          ],
          [
            Coordinate$AdventOfCode.stepNW(init),
            [
              3,
              3
            ]
          ],
          [
            Coordinate$AdventOfCode.stepSE(init),
            [
              5,
              5
            ]
          ],
          [
            Coordinate$AdventOfCode.stepSW(init),
            [
              3,
              5
            ]
          ]
        ];
        Jest2$AdventOfCode.testEach2("Single Step", singleStep_tests, (function (result, expected) {
                expect(result).toEqual(expected);
                
              }));
        var multipleStep_test = [
          [
            Coordinate$AdventOfCode.stepN(Coordinate$AdventOfCode.stepN(init)),
            [
              4,
              2
            ]
          ],
          [
            Coordinate$AdventOfCode.stepE(Coordinate$AdventOfCode.stepE(init)),
            [
              6,
              4
            ]
          ],
          [
            Coordinate$AdventOfCode.stepW(Coordinate$AdventOfCode.stepS(init)),
            [
              3,
              5
            ]
          ],
          [
            Coordinate$AdventOfCode.stepSW(Coordinate$AdventOfCode.stepNE(init)),
            [
              4,
              4
            ]
          ],
          [
            Coordinate$AdventOfCode.stepSE(Coordinate$AdventOfCode.stepNW(init)),
            [
              4,
              4
            ]
          ]
        ];
        return Jest2$AdventOfCode.testEach2("Multiple Step", multipleStep_test, (function (result, expected) {
                      expect(result).toEqual(expected);
                      
                    }));
      }));

/*  Not a pure module */
