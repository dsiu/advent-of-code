// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as AOC2020_Day3$AdventOfCode from "./AOC2020_Day3.mjs";
import * as AOC2020_Day3_Data$AdventOfCode from "./AOC2020_Day3_Data.mjs";
import * as AOC2020_Day3_Data_Sample$AdventOfCode from "./AOC2020_Day3_Data_Sample.mjs";

Jest.describe("2020 Day3", (function () {
        Jest.test("Part 1 - Test Data", (function () {
                var parsed = Belt_Array.map(AOC2020_Day3_Data_Sample$AdventOfCode.data.split("\n"), (function (prim) {
                        return prim.trim();
                      }));
                var result = AOC2020_Day3$AdventOfCode.TreeMap.walk(AOC2020_Day3$AdventOfCode.TreeMap.make(parsed), [
                      3,
                      1
                    ]);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 7);
              }));
        Jest.test("Part 1 - Solve", (function () {
                var parsed = Belt_Array.map(AOC2020_Day3_Data$AdventOfCode.data.split("\n"), (function (prim) {
                        return prim.trim();
                      }));
                var result = AOC2020_Day3$AdventOfCode.TreeMap.walk(AOC2020_Day3$AdventOfCode.TreeMap.make(parsed), [
                      3,
                      1
                    ]);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 162);
              }));
        Jest.test("Part 2 - Solve", (function () {
                var result = AOC2020_Day3$AdventOfCode.solvePart2(AOC2020_Day3_Data$AdventOfCode.data);
                return Jest.Expect.toBe(Jest.Expect.expect(result), "3064612320");
              }));
      }));

var data = AOC2020_Day3_Data$AdventOfCode.data;

var sampleData = AOC2020_Day3_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
}
/*  Not a pure module */
