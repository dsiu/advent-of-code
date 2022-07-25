// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2018_Day6$AdventOfCode from "./AOC2018_Day6.mjs";
import * as AOC2018_Day6_Data$AdventOfCode from "./AOC2018_Day6_Data.mjs";
import * as AOC2018_Day6_Data_Sample$AdventOfCode from "./AOC2018_Day6_Data_Sample.mjs";

Jest.describe("2018 Day6", (function (param) {
        Jest.describe("Part 1", (function (param) {
                Jest.test("Parse Coord", (function (param) {
                        var expected = [
                          AOC2018_Day6$AdventOfCode.Coord.make(1, 2),
                          AOC2018_Day6$AdventOfCode.Coord.make(3, 4),
                          AOC2018_Day6$AdventOfCode.Coord.make(5, 6)
                        ];
                        var result = AOC2018_Day6$AdventOfCode.Coord.parseCoords("1,2\n      3,4\n      5,6".split("\n"));
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("maxXY", (function (param) {
                        var expected = AOC2018_Day6$AdventOfCode.Coord.make(5, 8);
                        var result = AOC2018_Day6$AdventOfCode.Coord.maxXY(AOC2018_Day6$AdventOfCode.Coord.parseCoords("0,2\n      3,8\n      5,6".split("\n")));
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("minXY", (function (param) {
                        var expected = AOC2018_Day6$AdventOfCode.Coord.make(0, 1);
                        var result = AOC2018_Day6$AdventOfCode.Coord.minXY(AOC2018_Day6$AdventOfCode.Coord.parseCoords("0,2\n          3,8\n          5,1".split("\n")));
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
                      }));
                Jest.test("dist", (function (param) {
                        var a = AOC2018_Day6$AdventOfCode.Coord.make(1, 1);
                        var b = AOC2018_Day6$AdventOfCode.Coord.make(14, 27);
                        var result = AOC2018_Day6$AdventOfCode.Coord.dist(a, b);
                        return Jest.Expect.toEqual(Jest.Expect.expect(result), 39);
                      }));
                return Jest.Skip.test("Solve Part 1", (function (param) {
                              return Jest.Expect.toEqual(Jest.Expect.expect(1), 1);
                            }));
              }));
        return Jest.describe("Part 2", (function (param) {
                      return Jest.Skip.test("Solve Part 2", (function (param) {
                                    return Jest.Expect.toEqual(Jest.Expect.expect(1), 1);
                                  }));
                    }));
      }));

var data = AOC2018_Day6_Data$AdventOfCode.data;

var sampleData = AOC2018_Day6_Data_Sample$AdventOfCode.data;

export {
  data ,
  sampleData ,
  
}
/*  Not a pure module */