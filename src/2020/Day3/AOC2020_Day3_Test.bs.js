// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.bs.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var AOC2020_Day3$AdventOfCode = require("./AOC2020_Day3.bs.js");
var AOC2020_Day3_Data$AdventOfCode = require("./AOC2020_Day3_Data.bs.js");
var AOC2020_Day3_Data_Sample$AdventOfCode = require("./AOC2020_Day3_Data_Sample.bs.js");

Jest.describe("2020 Day3", (function (param) {
        Jest.test("Part 1 - Test Data", (function (param) {
                var parsed = Belt_Array.map(AOC2020_Day3_Data_Sample$AdventOfCode.data.split("\n"), (function (prim) {
                        return prim.trim();
                      }));
                var result = AOC2020_Day3$AdventOfCode.TreeMap.walk(AOC2020_Day3$AdventOfCode.TreeMap.make(parsed), [
                      3,
                      1
                    ]);
                return Jest.Expect.toEqual(7, Jest.Expect.expect(result));
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var parsed = Belt_Array.map(AOC2020_Day3_Data$AdventOfCode.data.split("\n"), (function (prim) {
                        return prim.trim();
                      }));
                var result = AOC2020_Day3$AdventOfCode.TreeMap.walk(AOC2020_Day3$AdventOfCode.TreeMap.make(parsed), [
                      3,
                      1
                    ]);
                return Jest.Expect.toEqual(162, Jest.Expect.expect(result));
              }));
        return Jest.test("Part 2 - Solve", (function (param) {
                      var result = AOC2020_Day3$AdventOfCode.solvePart2(AOC2020_Day3_Data$AdventOfCode.data);
                      return Jest.Expect.toBe("3064612320", Jest.Expect.expect(result));
                    }));
      }));

var data = AOC2020_Day3_Data$AdventOfCode.data;

var sampleData = AOC2020_Day3_Data_Sample$AdventOfCode.data;

exports.data = data;
exports.sampleData = sampleData;
/*  Not a pure module */
