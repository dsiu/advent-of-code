// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Jest2$AdventOfCode from "../../../interop/Jest2.mjs";
import * as Utils$AdventOfCode from "../../Utils.mjs";
import * as AOC2021_Day4$AdventOfCode from "./AOC2021_Day4.mjs";
import * as AOC2021_Day4_Data$AdventOfCode from "./AOC2021_Day4_Data.mjs";
import * as AOC2021_Day4_Data_Sample$AdventOfCode from "./AOC2021_Day4_Data_Sample.mjs";

Jest.describe("2021 Day4", (function (param) {
        Jest.describe("board", (function (param) {
                var board = AOC2021_Day4$AdventOfCode.Board.make(Utils$AdventOfCode.splitNewline("14 21 17 24  4\n             10 16 15  9 19\n             18  8 23 26 20\n             22 11 13  6  5\n             2  0 12  3  7"));
                var board_tests = [
                  [
                    AOC2021_Day4$AdventOfCode.Board.solve(board, [
                          7,
                          3,
                          0,
                          12,
                          2
                        ]),
                    [
                      4,
                      5,
                      6,
                      8,
                      9,
                      10,
                      11,
                      13,
                      14,
                      15,
                      16,
                      17,
                      18,
                      19,
                      20,
                      21,
                      22,
                      23,
                      24,
                      26
                    ]
                  ],
                  [
                    AOC2021_Day4$AdventOfCode.Board.solve(board, [
                          99,
                          7,
                          3,
                          0,
                          2,
                          12,
                          13
                        ]),
                    [
                      4,
                      5,
                      6,
                      8,
                      9,
                      10,
                      11,
                      14,
                      15,
                      16,
                      17,
                      18,
                      19,
                      20,
                      21,
                      22,
                      23,
                      24,
                      26
                    ]
                  ],
                  [
                    AOC2021_Day4$AdventOfCode.Board.solve(board, [
                          24,
                          9,
                          6,
                          3,
                          26
                        ]),
                    [
                      0,
                      2,
                      4,
                      5,
                      7,
                      8,
                      10,
                      11,
                      12,
                      13,
                      14,
                      15,
                      16,
                      17,
                      18,
                      19,
                      20,
                      21,
                      22,
                      23
                    ]
                  ],
                  [
                    AOC2021_Day4$AdventOfCode.Board.solve(board, [
                          24,
                          9,
                          12,
                          6,
                          3,
                          26
                        ]),
                    [
                      0,
                      2,
                      4,
                      5,
                      7,
                      8,
                      10,
                      11,
                      13,
                      14,
                      15,
                      16,
                      17,
                      18,
                      19,
                      20,
                      21,
                      22,
                      23
                    ]
                  ]
                ];
                Jest2$AdventOfCode.testEach2("board tests", board_tests, (function (result, expected) {
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
              }));
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2021_Day4$AdventOfCode.solvePart1(AOC2021_Day4_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 4512);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2021_Day4$AdventOfCode.solvePart1(AOC2021_Day4_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 41503);
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2021_Day4$AdventOfCode.solvePart2(AOC2021_Day4_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 1924);
              }));
        Jest.test("Part 2 - Solve", (function (param) {
                var result = AOC2021_Day4$AdventOfCode.solvePart2(AOC2021_Day4_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 3178);
              }));
      }));

var data = AOC2021_Day4_Data$AdventOfCode.data;

var sampleData = AOC2021_Day4_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
