// Generated by ReScript, PLEASE EDIT WITH CARE

import Ms from "ms";
import * as AOC2020_Day16$AdventOfCode from "./AOC2020_Day16.mjs";
import * as AOC2020_Day16_Data$AdventOfCode from "./AOC2020_Day16_Data.mjs";
import * as AOC2020_Day16_Data_Sample$AdventOfCode from "./AOC2020_Day16_Data_Sample.mjs";

function log(prim) {
  console.log(prim);
}

var t1 = Date.now();

var part1 = AOC2020_Day16$AdventOfCode.solvePart1(AOC2020_Day16_Data$AdventOfCode.data);

var t1_done = Date.now();

console.log("Part 1 Result");

console.log(part1);

var prim = "time : " + Ms(t1_done - t1) + "";

console.log(prim);

console.log("----------");

var t2 = Date.now();

var part2 = AOC2020_Day16$AdventOfCode.solvePart2(AOC2020_Day16_Data$AdventOfCode.data);

var t2_done = Date.now();

console.log("Part 2 Result");

console.log(part2);

var prim$1 = "time : " + Ms(t1_done - t1) + "";

console.log(prim$1);

var data = AOC2020_Day16_Data$AdventOfCode.data;

var sampleData = AOC2020_Day16_Data_Sample$AdventOfCode.data;

var solvePart1 = AOC2020_Day16$AdventOfCode.solvePart1;

var solvePart2 = AOC2020_Day16$AdventOfCode.solvePart2;

export {
  log ,
  data ,
  sampleData ,
  solvePart1 ,
  solvePart2 ,
  t1 ,
  part1 ,
  t1_done ,
  t2 ,
  part2 ,
  t2_done ,
}
/* t1 Not a pure module */
