// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as List from "rescript/lib/es6/List.js";
import * as AOC2021_Day17$AdventOfCode from "./AOC2021_Day17.mjs";
import * as AOC2021_Day17_Data$AdventOfCode from "./AOC2021_Day17_Data.mjs";
import * as AOC2021_Day17_Data_Sample$AdventOfCode from "./AOC2021_Day17_Data_Sample.mjs";

Jest.describe("2021 Day17", () => {
  Jest.describe("launch", () => {
    let t = AOC2021_Day17$AdventOfCode.parse(AOC2021_Day17_Data_Sample$AdventOfCode.data);
    let makeLaunchTests = __x => List.map(__x, param => [
      AOC2021_Day17$AdventOfCode.TrickShot.launch(param[0], t),
      param[1]
    ]);
    let example_tests = makeLaunchTests({
      hd: [
        {
          x: 7,
          y: 2
        },
        {
          TAG: "Hit",
          _0: [
            28,
            -7
          ],
          _1: [
            [
              0,
              0
            ],
            [
              7,
              2
            ],
            [
              13,
              3
            ],
            [
              18,
              3
            ],
            [
              22,
              2
            ],
            [
              25,
              0
            ],
            [
              27,
              -3
            ]
          ]
        }
      ],
      tl: {
        hd: [
          {
            x: 6,
            y: 3
          },
          {
            TAG: "Hit",
            _0: [
              21,
              -9
            ],
            _1: [
              [
                0,
                0
              ],
              [
                6,
                3
              ],
              [
                11,
                5
              ],
              [
                15,
                6
              ],
              [
                18,
                6
              ],
              [
                20,
                5
              ],
              [
                21,
                3
              ],
              [
                21,
                0
              ],
              [
                21,
                -4
              ]
            ]
          }
        ],
        tl: {
          hd: [
            {
              x: 9,
              y: 0
            },
            {
              TAG: "Hit",
              _0: [
                30,
                -6
              ],
              _1: [
                [
                  0,
                  0
                ],
                [
                  9,
                  0
                ],
                [
                  17,
                  -1
                ],
                [
                  24,
                  -3
                ]
              ]
            }
          ],
          tl: {
            hd: [
              {
                x: 17,
                y: -4
              },
              {
                TAG: "Miss",
                _0: [
                  [
                    0,
                    0
                  ],
                  [
                    17,
                    -4
                  ],
                  [
                    33,
                    -9
                  ]
                ]
              }
            ],
            tl: {
              hd: [
                {
                  x: 6,
                  y: 9
                },
                {
                  TAG: "Hit",
                  _0: [
                    21,
                    -10
                  ],
                  _1: [
                    [
                      0,
                      0
                    ],
                    [
                      6,
                      9
                    ],
                    [
                      11,
                      17
                    ],
                    [
                      15,
                      24
                    ],
                    [
                      18,
                      30
                    ],
                    [
                      20,
                      35
                    ],
                    [
                      21,
                      39
                    ],
                    [
                      21,
                      42
                    ],
                    [
                      21,
                      44
                    ],
                    [
                      21,
                      45
                    ],
                    [
                      21,
                      45
                    ],
                    [
                      21,
                      44
                    ],
                    [
                      21,
                      42
                    ],
                    [
                      21,
                      39
                    ],
                    [
                      21,
                      35
                    ],
                    [
                      21,
                      30
                    ],
                    [
                      21,
                      24
                    ],
                    [
                      21,
                      17
                    ],
                    [
                      21,
                      9
                    ],
                    [
                      21,
                      0
                    ]
                  ]
                }
              ],
              tl: /* [] */0
            }
          }
        }
      }
    });
    Jest.testAll("examples", example_tests, param => Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]));
  });
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2021_Day17$AdventOfCode.solvePart1(AOC2021_Day17_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 45);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2021_Day17$AdventOfCode.solvePart1(AOC2021_Day17_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 5050);
  });
  Jest.test("Part 2 - Sample Data", () => {
    let result = AOC2021_Day17$AdventOfCode.solvePart2(AOC2021_Day17_Data_Sample$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 112);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2021_Day17$AdventOfCode.solvePart2(AOC2021_Day17_Data$AdventOfCode.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2223);
  });
});

let data = AOC2021_Day17_Data$AdventOfCode.data;

let sampleData = AOC2021_Day17_Data_Sample$AdventOfCode.data;

let solvePart1 = AOC2021_Day17$AdventOfCode.solvePart1;

let solvePart2 = AOC2021_Day17$AdventOfCode.solvePart2;

let parse = AOC2021_Day17$AdventOfCode.parse;

export {
  data,
  sampleData,
  solvePart1,
  solvePart2,
  parse,
}
/*  Not a pure module */
