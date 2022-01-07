// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest2$AdventOfCode = require("../../../interop/Jest2.bs.js");
var AOC2021_Day5$AdventOfCode = require("./AOC2021_Day5.bs.js");
var AOC2021_Day5_Data$AdventOfCode = require("./AOC2021_Day5_Data.bs.js");
var AOC2021_Day5_Data_Sample$AdventOfCode = require("./AOC2021_Day5_Data_Sample.bs.js");

describe("2020 DayX", (function () {
        test("Part 1 - Sample Data", (function () {
                var result = AOC2021_Day5$AdventOfCode.solvePart1(AOC2021_Day5_Data_Sample$AdventOfCode.data);
                expect(result).toEqual(5);
                
              }));
        test("Part 1 - Solve", (function () {
                var result = AOC2021_Day5$AdventOfCode.solvePart1(AOC2021_Day5_Data$AdventOfCode.data);
                expect(result).toEqual(7085);
                
              }));
        var point_tests = [
          [
            AOC2021_Day5$AdventOfCode.Line.makePoints({
                  x: 0,
                  y: 0
                }, {
                  x: 0,
                  y: 2
                }),
            [
              {
                x: 0,
                y: 0
              },
              {
                x: 0,
                y: 1
              },
              {
                x: 0,
                y: 2
              }
            ]
          ],
          [
            AOC2021_Day5$AdventOfCode.Line.makePoints({
                  x: 3,
                  y: 3
                }, {
                  x: 5,
                  y: 3
                }),
            [
              {
                x: 3,
                y: 3
              },
              {
                x: 4,
                y: 3
              },
              {
                x: 5,
                y: 3
              }
            ]
          ],
          [
            AOC2021_Day5$AdventOfCode.Line.makePoints({
                  x: 9,
                  y: 7
                }, {
                  x: 7,
                  y: 9
                }),
            [
              {
                x: 9,
                y: 7
              },
              {
                x: 8,
                y: 8
              },
              {
                x: 7,
                y: 9
              }
            ]
          ]
        ];
        Jest2$AdventOfCode.testEach2("makePoints", point_tests, (function (result, expected) {
                expect(result).toEqual(expected);
                
              }));
        test("Part 2 - Sample Data", (function () {
                var result = AOC2021_Day5$AdventOfCode.solvePart2(AOC2021_Day5_Data_Sample$AdventOfCode.data);
                expect(result).toEqual(12);
                
              }));
        test("Part 2 - Solve", (function () {
                var result = AOC2021_Day5$AdventOfCode.solvePart2(AOC2021_Day5_Data$AdventOfCode.data);
                expect(result).toEqual(20271);
                
              }));
        
      }));

var data = AOC2021_Day5_Data$AdventOfCode.data;

var sampleData = AOC2021_Day5_Data_Sample$AdventOfCode.data;

exports.data = data;
exports.sampleData = sampleData;
/*  Not a pure module */
