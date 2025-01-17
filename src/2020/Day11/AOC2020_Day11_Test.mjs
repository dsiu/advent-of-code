// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Coordinate$AdventOfCode from "../../Coordinate.mjs";
import * as AOC2020_Day11$AdventOfCode from "./AOC2020_Day11.mjs";
import * as AOC2020_Day11_Data$AdventOfCode from "./AOC2020_Day11_Data.mjs";
import * as AOC2020_Day11_Data_Sample$AdventOfCode from "./AOC2020_Day11_Data_Sample.mjs";

Jest.describe("2020 Day11", (function () {
        var seats = AOC2020_Day11$AdventOfCode.parse(AOC2020_Day11_Data_Sample$AdventOfCode.data);
        Jest.describe("SeatMap", (function () {
                var getAdj_tests_0 = [
                  AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
                        0,
                        0
                      ]),
                  [
                    ".",
                    "L",
                    "L"
                  ]
                ];
                var getAdj_tests_1 = {
                  hd: [
                    AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
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
                      AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
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
                        AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
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
                          AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
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
                            AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
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
                              AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
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
                                AOC2020_Day11$AdventOfCode.SeatMap.getAdjacents(seats, [
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
                var getAdj_tests = {
                  hd: getAdj_tests_0,
                  tl: getAdj_tests_1
                };
                Jest.testAll("getAdjacents", getAdj_tests, (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
                      }));
              }));
        Jest.describe("next seat given a direction", (function () {
                var map = AOC2020_Day11$AdventOfCode.parse(".......#.\n                 ...#.....\n                 .#.......\n                 .........\n                 ..#L....#\n                 ....#....\n                 .........\n                 #........\n                 ...#.....");
                var init_1 = [
                  3,
                  4
                ];
                var nextSeat_test1_0 = [
                  AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepN),
                  "#"
                ];
                var nextSeat_test1_1 = {
                  hd: [
                    AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepE),
                    "#"
                  ],
                  tl: {
                    hd: [
                      AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepS),
                      "#"
                    ],
                    tl: {
                      hd: [
                        AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepW),
                        "#"
                      ],
                      tl: {
                        hd: [
                          AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepNE),
                          "#"
                        ],
                        tl: {
                          hd: [
                            AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepNW),
                            "#"
                          ],
                          tl: {
                            hd: [
                              AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepSE),
                              "#"
                            ],
                            tl: {
                              hd: [
                                AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_1, Coordinate$AdventOfCode.StepFunctions.stepSW),
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
                var nextSeat_test1 = {
                  hd: nextSeat_test1_0,
                  tl: nextSeat_test1_1
                };
                Jest.testAll("test 1", nextSeat_test1, (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
                      }));
                var init_2 = [
                  3,
                  5
                ];
                var nextSeat_test2_0 = [
                  AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepN),
                  "L"
                ];
                var nextSeat_test2_1 = {
                  hd: [
                    AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepE),
                    "#"
                  ],
                  tl: {
                    hd: [
                      AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepS),
                      "#"
                    ],
                    tl: {
                      hd: [
                        AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepW),
                        "."
                      ],
                      tl: {
                        hd: [
                          AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepNE),
                          "."
                        ],
                        tl: {
                          hd: [
                            AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepNW),
                            "#"
                          ],
                          tl: {
                            hd: [
                              AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepSE),
                              "."
                            ],
                            tl: {
                              hd: [
                                AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_2, Coordinate$AdventOfCode.StepFunctions.stepSW),
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
                var nextSeat_test2 = {
                  hd: nextSeat_test2_0,
                  tl: nextSeat_test2_1
                };
                Jest.testAll("test 2", nextSeat_test2, (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
                      }));
                var init_3 = [
                  0,
                  0
                ];
                var nextSeat_test3_0 = [
                  AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepN),
                  "."
                ];
                var nextSeat_test3_1 = {
                  hd: [
                    AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepE),
                    "#"
                  ],
                  tl: {
                    hd: [
                      AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepS),
                      "#"
                    ],
                    tl: {
                      hd: [
                        AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepW),
                        "."
                      ],
                      tl: {
                        hd: [
                          AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepNE),
                          "."
                        ],
                        tl: {
                          hd: [
                            AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepNW),
                            "."
                          ],
                          tl: {
                            hd: [
                              AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepSE),
                              "."
                            ],
                            tl: {
                              hd: [
                                AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_3, Coordinate$AdventOfCode.StepFunctions.stepSW),
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
                var nextSeat_test3 = {
                  hd: nextSeat_test3_0,
                  tl: nextSeat_test3_1
                };
                Jest.testAll("test 3", nextSeat_test3, (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
                      }));
                var init_4 = [
                  4,
                  4
                ];
                var nextSeat_test4_0 = [
                  AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepN),
                  "."
                ];
                var nextSeat_test4_1 = {
                  hd: [
                    AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepE),
                    "#"
                  ],
                  tl: {
                    hd: [
                      AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepS),
                      "#"
                    ],
                    tl: {
                      hd: [
                        AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepW),
                        "L"
                      ],
                      tl: {
                        hd: [
                          AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepNE),
                          "."
                        ],
                        tl: {
                          hd: [
                            AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepNW),
                            "."
                          ],
                          tl: {
                            hd: [
                              AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepSE),
                              "."
                            ],
                            tl: {
                              hd: [
                                AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map, init_4, Coordinate$AdventOfCode.StepFunctions.stepSW),
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
                var nextSeat_test4 = {
                  hd: nextSeat_test4_0,
                  tl: nextSeat_test4_1
                };
                Jest.testAll("test 4", nextSeat_test4, (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
                      }));
                var map_2 = AOC2020_Day11$AdventOfCode.parse(".............\n                  .L.L.#.#.#.#.\n                  .............");
                var nextSeat_test5_0 = [
                  AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map_2, [
                        1,
                        1
                      ], Coordinate$AdventOfCode.StepFunctions.stepE),
                  "L"
                ];
                var nextSeat_test5_1 = {
                  hd: [
                    AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map_2, [
                          3,
                          1
                        ], Coordinate$AdventOfCode.StepFunctions.stepE),
                    "#"
                  ],
                  tl: {
                    hd: [
                      AOC2020_Day11$AdventOfCode.SeatMap.nextSeatIn(map_2, [
                            3,
                            1
                          ], Coordinate$AdventOfCode.StepFunctions.stepW),
                      "L"
                    ],
                    tl: /* [] */0
                  }
                };
                var nextSeat_test5 = {
                  hd: nextSeat_test5_0,
                  tl: nextSeat_test5_1
                };
                Jest.testAll("test 5", nextSeat_test5, (function (param) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(param[0]), param[1]);
                      }));
              }));
        Jest.test("Part 1 - Sample Data", (function () {
                var result = AOC2020_Day11$AdventOfCode.solvePart1(AOC2020_Day11_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 37);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var result = AOC2020_Day11$AdventOfCode.solvePart1(AOC2020_Day11_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 2270);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2020_Day11$AdventOfCode.solvePart2(AOC2020_Day11_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 2042);
              }));
      }));

var data = AOC2020_Day11_Data$AdventOfCode.data;

var sampleData = AOC2020_Day11_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
