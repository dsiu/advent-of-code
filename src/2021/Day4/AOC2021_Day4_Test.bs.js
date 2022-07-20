// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest2$AdventOfCode = require("../../../interop/Jest2.bs.js");
var Utils$AdventOfCode = require("../../Utils.bs.js");
var AOC2021_Day4$AdventOfCode = require("./AOC2021_Day4.bs.js");
var AOC2021_Day4_Data$AdventOfCode = require("./AOC2021_Day4_Data.bs.js");
var AOC2021_Day4_Data_Sample$AdventOfCode = require("./AOC2021_Day4_Data_Sample.bs.js");

describe("2021 Day4", (function () {
        describe("board", (function () {
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
                return Jest2$AdventOfCode.testEach2("board tests", board_tests, (function (result, expected) {
                              expect(result).toEqual(expected);
                              
                            }));
              }));
        test("Part 1 - Sample Data", (function () {
                var result = AOC2021_Day4$AdventOfCode.solvePart1(AOC2021_Day4_Data_Sample$AdventOfCode.data);
                expect(result).toEqual(4512);
                
              }));
        test("Part 1 - Solve", (function () {
                var result = AOC2021_Day4$AdventOfCode.solvePart1(AOC2021_Day4_Data$AdventOfCode.data);
                expect(result).toEqual(41503);
                
              }));
        test("Part 2 - Sample Data", (function () {
                var result = AOC2021_Day4$AdventOfCode.solvePart2(AOC2021_Day4_Data_Sample$AdventOfCode.data);
                expect(result).toEqual(1924);
                
              }));
        test("Part 2 - Solve", (function () {
                var result = AOC2021_Day4$AdventOfCode.solvePart2(AOC2021_Day4_Data$AdventOfCode.data);
                expect(result).toEqual(3178);
                
              }));
        
      }));

var data = AOC2021_Day4_Data$AdventOfCode.data;

var sampleData = AOC2021_Day4_Data_Sample$AdventOfCode.data;

exports.data = data;
exports.sampleData = sampleData;
/*  Not a pure module */
