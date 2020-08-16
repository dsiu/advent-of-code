// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Jest = require("@glennsl/bs-jest/src/jest.js");
var Day1$AdventOfCode = require("../../../src/2018/Day1/Day1.bs.js");
var Day1_Data$AdventOfCode = require("../../../src/2018/Day1/Day1_Data.bs.js");

Jest.describe("2018 Day1", (function (param) {
        var part2Result = Day1$AdventOfCode.runDay1Part2(Day1_Data$AdventOfCode.data);
        return Jest.test("Part 2 solve", (function (param) {
                      return Jest.Expect.toBe(75108, Jest.Expect.expect(part2Result.found));
                    }));
      }));

/*  Not a pure module */
