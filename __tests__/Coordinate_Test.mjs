// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Coordinate$AdventOfCode from "../src/Coordinate.mjs";

Jest.describe("Step Functions", (function (param) {
        var init = [
          4,
          4
        ];
        var singleStep_tests_0 = [
          Coordinate$AdventOfCode.StepFunctions.stepN(init),
          [
            4,
            3
          ]
        ];
        var singleStep_tests_1 = {
          hd: [
            Coordinate$AdventOfCode.StepFunctions.stepE(init),
            [
              5,
              4
            ]
          ],
          tl: {
            hd: [
              Coordinate$AdventOfCode.StepFunctions.stepS(init),
              [
                4,
                5
              ]
            ],
            tl: {
              hd: [
                Coordinate$AdventOfCode.StepFunctions.stepW(init),
                [
                  3,
                  4
                ]
              ],
              tl: {
                hd: [
                  Coordinate$AdventOfCode.StepFunctions.stepNE(init),
                  [
                    5,
                    3
                  ]
                ],
                tl: {
                  hd: [
                    Coordinate$AdventOfCode.StepFunctions.stepNW(init),
                    [
                      3,
                      3
                    ]
                  ],
                  tl: {
                    hd: [
                      Coordinate$AdventOfCode.StepFunctions.stepSE(init),
                      [
                        5,
                        5
                      ]
                    ],
                    tl: {
                      hd: [
                        Coordinate$AdventOfCode.StepFunctions.stepSW(init),
                        [
                          3,
                          5
                        ]
                      ],
                      tl: /* [] */0
                    }
                  }
                }
              }
            }
          }
        };
        var singleStep_tests = {
          hd: singleStep_tests_0,
          tl: singleStep_tests_1
        };
        Jest.testAll("Single Step", singleStep_tests, (function (param) {
                return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
              }));
        var __x = Coordinate$AdventOfCode.StepFunctions.stepN(init);
        var __x$1 = Coordinate$AdventOfCode.StepFunctions.stepE(init);
        var __x$2 = Coordinate$AdventOfCode.StepFunctions.stepS(init);
        var __x$3 = Coordinate$AdventOfCode.StepFunctions.stepNE(init);
        var __x$4 = Coordinate$AdventOfCode.StepFunctions.stepNW(init);
        var multipleStep_test_0 = [
          Coordinate$AdventOfCode.StepFunctions.stepN(__x),
          [
            4,
            2
          ]
        ];
        var multipleStep_test_1 = {
          hd: [
            Coordinate$AdventOfCode.StepFunctions.stepE(__x$1),
            [
              6,
              4
            ]
          ],
          tl: {
            hd: [
              Coordinate$AdventOfCode.StepFunctions.stepW(__x$2),
              [
                3,
                5
              ]
            ],
            tl: {
              hd: [
                Coordinate$AdventOfCode.StepFunctions.stepSW(__x$3),
                [
                  4,
                  4
                ]
              ],
              tl: {
                hd: [
                  Coordinate$AdventOfCode.StepFunctions.stepSE(__x$4),
                  [
                    4,
                    4
                  ]
                ],
                tl: /* [] */0
              }
            }
          }
        };
        var multipleStep_test = {
          hd: multipleStep_test_0,
          tl: multipleStep_test_1
        };
        Jest.testAll("Multiple Steps", multipleStep_test, (function (param) {
                return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
              }));
      }));

export {
  
}
/*  Not a pure module */
