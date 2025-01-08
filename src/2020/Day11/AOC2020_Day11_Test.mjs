// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Coordinate from "../../Coordinate.mjs";
import * as AOC2020_Day11 from "./AOC2020_Day11.mjs";
import * as AOC2020_Day11_Data from "./AOC2020_Day11_Data.mjs";
import * as AOC2020_Day11_Data_Sample from "./AOC2020_Day11_Data_Sample.mjs";

Jest.describe("2020 Day11", () => {
  let seats = AOC2020_Day11.parse(AOC2020_Day11_Data_Sample.data);
  Jest.describe("SeatMap", () => {
    let getAdj_tests_0 = [
      AOC2020_Day11.SeatMap.getAdjacents(seats, [
        0,
        0
      ]),
      [
        ".",
        "L",
        "L"
      ]
    ];
    let getAdj_tests_1 = {
      hd: [
        AOC2020_Day11.SeatMap.getAdjacents(seats, [
          1,
          0
        ]),
        [
          "L",
          "L",
          "L",
          "L",
          "L"
        ]
      ],
      tl: {
        hd: [
          AOC2020_Day11.SeatMap.getAdjacents(seats, [
            2,
            0
          ]),
          [
            ".",
            "L",
            "L",
            "L",
            "L"
          ]
        ],
        tl: {
          hd: [
            AOC2020_Day11.SeatMap.getAdjacents(seats, [
              9,
              0
            ]),
            [
              "L",
              "L",
              "L"
            ]
          ],
          tl: {
            hd: [
              AOC2020_Day11.SeatMap.getAdjacents(seats, [
                2,
                1
              ]),
              [
                ".",
                "L",
                "L",
                "L",
                "L",
                ".",
                "L",
                "."
              ]
            ],
            tl: {
              hd: [
                AOC2020_Day11.SeatMap.getAdjacents(seats, [
                  0,
                  9
                ]),
                [
                  "L",
                  ".",
                  "."
                ]
              ],
              tl: {
                hd: [
                  AOC2020_Day11.SeatMap.getAdjacents(seats, [
                    8,
                    9
                  ]),
                  [
                    "L",
                    ".",
                    "L",
                    ".",
                    "L"
                  ]
                ],
                tl: {
                  hd: [
                    AOC2020_Day11.SeatMap.getAdjacents(seats, [
                      9,
                      9
                    ]),
                    [
                      ".",
                      "L",
                      "L"
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
    let getAdj_tests = {
      hd: getAdj_tests_0,
      tl: getAdj_tests_1
    };
    Jest.testAll("getAdjacents", getAdj_tests, param => Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]));
  });
  Jest.describe("next seat given a direction", () => {
    let map = AOC2020_Day11.parse(".......#.\n                 ...#.....\n                 .#.......\n                 .........\n                 ..#L....#\n                 ....#....\n                 .........\n                 #........\n                 ...#.....");
    let init_1 = [
      3,
      4
    ];
    let nextSeat_test1_0 = [
      AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepN),
      "#"
    ];
    let nextSeat_test1_1 = {
      hd: [
        AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepE),
        "#"
      ],
      tl: {
        hd: [
          AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepS),
          "#"
        ],
        tl: {
          hd: [
            AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepW),
            "#"
          ],
          tl: {
            hd: [
              AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepNE),
              "#"
            ],
            tl: {
              hd: [
                AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepNW),
                "#"
              ],
              tl: {
                hd: [
                  AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepSE),
                  "#"
                ],
                tl: {
                  hd: [
                    AOC2020_Day11.SeatMap.nextSeatIn(map, init_1, Coordinate.StepFunctions.stepSW),
                    "#"
                  ],
                  tl: /* [] */0
                }
              }
            }
          }
        }
      }
    };
    let nextSeat_test1 = {
      hd: nextSeat_test1_0,
      tl: nextSeat_test1_1
    };
    Jest.testAll("test 1", nextSeat_test1, param => Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]));
    let init_2 = [
      3,
      5
    ];
    let nextSeat_test2_0 = [
      AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepN),
      "L"
    ];
    let nextSeat_test2_1 = {
      hd: [
        AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepE),
        "#"
      ],
      tl: {
        hd: [
          AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepS),
          "#"
        ],
        tl: {
          hd: [
            AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepW),
            "."
          ],
          tl: {
            hd: [
              AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepNE),
              "."
            ],
            tl: {
              hd: [
                AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepNW),
                "#"
              ],
              tl: {
                hd: [
                  AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepSE),
                  "."
                ],
                tl: {
                  hd: [
                    AOC2020_Day11.SeatMap.nextSeatIn(map, init_2, Coordinate.StepFunctions.stepSW),
                    "."
                  ],
                  tl: /* [] */0
                }
              }
            }
          }
        }
      }
    };
    let nextSeat_test2 = {
      hd: nextSeat_test2_0,
      tl: nextSeat_test2_1
    };
    Jest.testAll("test 2", nextSeat_test2, param => Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]));
    let init_3 = [
      0,
      0
    ];
    let nextSeat_test3_0 = [
      AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepN),
      "."
    ];
    let nextSeat_test3_1 = {
      hd: [
        AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepE),
        "#"
      ],
      tl: {
        hd: [
          AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepS),
          "#"
        ],
        tl: {
          hd: [
            AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepW),
            "."
          ],
          tl: {
            hd: [
              AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepNE),
              "."
            ],
            tl: {
              hd: [
                AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepNW),
                "."
              ],
              tl: {
                hd: [
                  AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepSE),
                  "."
                ],
                tl: {
                  hd: [
                    AOC2020_Day11.SeatMap.nextSeatIn(map, init_3, Coordinate.StepFunctions.stepSW),
                    "."
                  ],
                  tl: /* [] */0
                }
              }
            }
          }
        }
      }
    };
    let nextSeat_test3 = {
      hd: nextSeat_test3_0,
      tl: nextSeat_test3_1
    };
    Jest.testAll("test 3", nextSeat_test3, param => Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]));
    let init_4 = [
      4,
      4
    ];
    let nextSeat_test4_0 = [
      AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepN),
      "."
    ];
    let nextSeat_test4_1 = {
      hd: [
        AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepE),
        "#"
      ],
      tl: {
        hd: [
          AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepS),
          "#"
        ],
        tl: {
          hd: [
            AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepW),
            "L"
          ],
          tl: {
            hd: [
              AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepNE),
              "."
            ],
            tl: {
              hd: [
                AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepNW),
                "."
              ],
              tl: {
                hd: [
                  AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepSE),
                  "."
                ],
                tl: {
                  hd: [
                    AOC2020_Day11.SeatMap.nextSeatIn(map, init_4, Coordinate.StepFunctions.stepSW),
                    "."
                  ],
                  tl: /* [] */0
                }
              }
            }
          }
        }
      }
    };
    let nextSeat_test4 = {
      hd: nextSeat_test4_0,
      tl: nextSeat_test4_1
    };
    Jest.testAll("test 4", nextSeat_test4, param => Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]));
    let map_2 = AOC2020_Day11.parse(".............\n                  .L.L.#.#.#.#.\n                  .............");
    let nextSeat_test5_0 = [
      AOC2020_Day11.SeatMap.nextSeatIn(map_2, [
        1,
        1
      ], Coordinate.StepFunctions.stepE),
      "L"
    ];
    let nextSeat_test5_1 = {
      hd: [
        AOC2020_Day11.SeatMap.nextSeatIn(map_2, [
          3,
          1
        ], Coordinate.StepFunctions.stepE),
        "#"
      ],
      tl: {
        hd: [
          AOC2020_Day11.SeatMap.nextSeatIn(map_2, [
            3,
            1
          ], Coordinate.StepFunctions.stepW),
          "L"
        ],
        tl: /* [] */0
      }
    };
    let nextSeat_test5 = {
      hd: nextSeat_test5_0,
      tl: nextSeat_test5_1
    };
    Jest.testAll("test 5", nextSeat_test5, param => Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]));
  });
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day11.solvePart1(AOC2020_Day11_Data_Sample.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 37);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2020_Day11.solvePart1(AOC2020_Day11_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2270);
  });
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2020_Day11.solvePart2(AOC2020_Day11_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 2042);
  });
});

let data = AOC2020_Day11_Data.data;

let sampleData = AOC2020_Day11_Data_Sample.data;

export {
  data,
  sampleData,
}
/*  Not a pure module */
