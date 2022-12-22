let log = Js.Console.log
let data = AOC2020_Day16_Data.data
let sampleData1 = AOC2020_Day16_Data_Sample.data1
let sampleData2 = AOC2020_Day16_Data_Sample.data2
let {solvePart1, solvePart2} = module(AOC2020_Day16)

Js.Console.timeStart("Part 1")

//let part1 = solvePart1(sampleData)
let part1 = solvePart1(data)

"Part 1 Result"->log
part1->log
Js.Console.timeEnd("Part 1")

"----------"->log

Js.Console.timeStart("Part 2")

//let part2 = solvePart2(sampleData)
let part2 = solvePart2(data)

"Part 2 Result"->log
part2->log
Js.Console.timeEnd("Part 2")
