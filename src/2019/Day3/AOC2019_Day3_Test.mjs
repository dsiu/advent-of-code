// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2019_Day3$AdventOfCode from "./AOC2019_Day3.mjs";
import * as AOC2019_Day3_Data$AdventOfCode from "./AOC2019_Day3_Data.mjs";
import * as AOC2019_Day3_Data_Sample$AdventOfCode from "./AOC2019_Day3_Data_Sample.mjs";

Jest.describe("2019 Day3", (function (param) {
        Jest.test("Part 1 - Sample Data", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart1(AOC2019_Day3_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 6);
              }));
        Jest.test("Part 1 - Sample Data 1", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart1(AOC2019_Day3_Data_Sample$AdventOfCode.data1);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 159);
              }));
        Jest.test("Part 1 - Sample Data 2", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart1(AOC2019_Day3_Data_Sample$AdventOfCode.data2);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 135);
              }));
        Jest.test("Part 1 - Solve", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart1(AOC2019_Day3_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 855);
              }));
        Jest.test("Part 2 - Sample Data", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart2(AOC2019_Day3_Data_Sample$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 30);
              }));
        Jest.test("Part 2 - Sample Data 1", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart2(AOC2019_Day3_Data_Sample$AdventOfCode.data1);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 610);
              }));
        Jest.test("Part 2 - Sample Data 2", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart2(AOC2019_Day3_Data_Sample$AdventOfCode.data2);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 410);
              }));
        Jest.test("Part 2 - Solve", (function (param) {
                var result = AOC2019_Day3$AdventOfCode.solvePart2(AOC2019_Day3_Data$AdventOfCode.data);
                return Jest.Expect.toEqual(Jest.Expect.expect(result), 11238);
              }));
      }));

var data = AOC2019_Day3_Data$AdventOfCode.data;

var sampleData = AOC2019_Day3_Data_Sample$AdventOfCode.data;

var sampleData1 = AOC2019_Day3_Data_Sample$AdventOfCode.data1;

var sampleData2 = AOC2019_Day3_Data_Sample$AdventOfCode.data2;

var solvePart1 = AOC2019_Day3$AdventOfCode.solvePart1;

var solvePart2 = AOC2019_Day3$AdventOfCode.solvePart2;

export {
  data ,
  sampleData ,
  sampleData1 ,
  sampleData2 ,
  solvePart1 ,
  solvePart2 ,
}
/*  Not a pure module */
