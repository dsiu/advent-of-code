// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as AOC2018_Day6 from "./AOC2018_Day6.mjs";
import * as AOC2018_Day6_Data from "./AOC2018_Day6_Data.mjs";
import * as AOC2018_Day6_Data_Sample from "./AOC2018_Day6_Data_Sample.mjs";

Jest.describe("2018 Day6", () => {
  Jest.describe("Part 1", () => {
    Jest.test("Parse Coord", () => {
      let expected = [
        AOC2018_Day6.Coord.make(1, 2),
        AOC2018_Day6.Coord.make(3, 4),
        AOC2018_Day6.Coord.make(5, 6)
      ];
      let result = AOC2018_Day6.Coord.parseCoords("1,2\n      3,4\n      5,6".split("\n"));
      return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
    });
    Jest.test("maxXY", () => {
      let expected = AOC2018_Day6.Coord.make(5, 8);
      let result = AOC2018_Day6.Coord.maxXY(AOC2018_Day6.Coord.parseCoords("0,2\n      3,8\n      5,6".split("\n")));
      return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
    });
    Jest.test("minXY", () => {
      let expected = AOC2018_Day6.Coord.make(0, 1);
      let result = AOC2018_Day6.Coord.minXY(AOC2018_Day6.Coord.parseCoords("0,2\n          3,8\n          5,1".split("\n")));
      return Jest.Expect.toEqual(Jest.Expect.expect(result), expected);
    });
    Jest.test("dist", () => {
      let a = AOC2018_Day6.Coord.make(1, 1);
      let b = AOC2018_Day6.Coord.make(14, 27);
      let result = AOC2018_Day6.Coord.dist(a, b);
      return Jest.Expect.toEqual(Jest.Expect.expect(result), 39);
    });
    Jest.Skip.test("Solve Part 1", () => Jest.Expect.toEqual(Jest.Expect.expect(1), 1));
  });
  Jest.describe("Part 2", () => Jest.Skip.test("Solve Part 2", () => Jest.Expect.toEqual(Jest.Expect.expect(2), 2)));
});

let data = AOC2018_Day6_Data.data;

let sampleData = AOC2018_Day6_Data_Sample.data;

export {
  data,
  sampleData,
}
/*  Not a pure module */
