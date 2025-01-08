// Generated by ReScript, PLEASE EDIT WITH CARE

import * as AOC2022_Day5 from "./AOC2022_Day5.mjs";
import * as AOC2022_Day5_Data from "./AOC2022_Day5_Data.mjs";
import * as AOC2022_Day5_Data_Sample from "./AOC2022_Day5_Data_Sample.mjs";

function log(prim) {
  console.log(prim);
}

console.time("Part 1");

let part1 = AOC2022_Day5.solvePart1(AOC2022_Day5_Data.data);

console.log("Part 1 Result");

console.log(part1);

console.timeEnd("Part 1");

console.log("----------");

console.time("Part 2");

let part2 = AOC2022_Day5.solvePart2(AOC2022_Day5_Data.data);

console.log("Part 2 Result");

console.log(part2);

console.timeEnd("Part 2");

let data = AOC2022_Day5_Data.data;

let sampleData = AOC2022_Day5_Data_Sample.data;

let solvePart1 = AOC2022_Day5.solvePart1;

let solvePart2 = AOC2022_Day5.solvePart2;

export {
  log,
  data,
  sampleData,
  solvePart1,
  solvePart2,
  part1,
  part2,
}
/*  Not a pure module */
