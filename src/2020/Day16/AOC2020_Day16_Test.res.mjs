// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.res.mjs";
import * as AOC2020_Day16 from "./AOC2020_Day16.res.mjs";
import * as AOC2020_Day16_Data from "./AOC2020_Day16_Data.res.mjs";
import * as AOC2020_Day16_Data_Sample from "./AOC2020_Day16_Data_Sample.res.mjs";

Jest.describe("2020 Day16", () => {
  Jest.test("Part 1 - Sample Data", () => {
    let result = AOC2020_Day16.solvePart1(AOC2020_Day16_Data_Sample.data1);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 71);
  });
  Jest.test("Part 1 - Solve", () => {
    let result = AOC2020_Day16.solvePart1(AOC2020_Day16_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 28882);
  });
  Jest.test("Part 2 - Sample Data", () => Jest.Expect.toEqual(Jest.Expect.expect(2), 2));
  Jest.test("Part 2 - Solve", () => {
    let result = AOC2020_Day16.solvePart2(AOC2020_Day16_Data.data);
    return Jest.Expect.toEqual(Jest.Expect.expect(result), 1429779530273);
  });
});

let data = AOC2020_Day16_Data.data;

let sampleData1 = AOC2020_Day16_Data_Sample.data1;

let sampleData2 = AOC2020_Day16_Data_Sample.data2;

export {
  data,
  sampleData1,
  sampleData2,
}
/*  Not a pure module */