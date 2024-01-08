@@uncurried

open Stdlib

let log = Console.log
let data = AOC2023_Day8_Data.data
let sampleData = AOC2023_Day8_Data_Sample.data
let sampleData2 = AOC2023_Day8_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2023_Day8)

Console.time("Part 1")

//let part1 = solvePart1(sampleData)
let part1 = solvePart1(data)

"Part 1 Result"->log
part1->log
Console.timeEnd("Part 1")

"----------"->log

Console.time("Part 2")

let part2 = solvePart2(sampleData2)
//let part2 = solvePart2(data)

"Part 2 Result"->log
part2->log
Console.timeEnd("Part 2")
